{-|
Module      : Qubism.QASM.ProgState
Description : Internal state of an OpenQASM program
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Qubism.QASM.ProgState where

-- For dependent typing
import GHC.TypeLits
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Finite

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Random
import Numeric.Natural

import Qubism.CReg
import Qubism.StateVec
import Qubism.QASM.AST

-- | This datatype encapsulates the concept of QReg's in QASM, which cannot
-- be considered independant. A QReg is a portion (perhaps the whole thing)
-- of a StateVec that is used in a calculation.
data QReg = QReg
  Id      -- ^ Identifier of the StateVec representing the full state
  Natural -- ^ Index of the first qubit
  Natural -- ^ Size of the register
  deriving Show

data SomeStateVec = forall n . KnownNat n => SomeSV (StateVec n)

instance Show SomeStateVec where
  show (SomeSV sv) = "\n" ++ show sv

data ProgState = ProgState
  { stVecs :: Map.Map Id SomeStateVec
  , qregs  :: Map.Map Id QReg
  , cregs  :: Map.Map Id CReg
  }

instance Show ProgState where
  show (ProgState sv qr cr) = "ProgState_________________\n"
    ++ "StateVecs: " ++ show sv ++ "\n"
    ++ "QRegs:     " ++ show qr ++ "\n"
    ++ "CRegs:     " ++ show cr ++ "\n"

blankState :: ProgState
blankState = ProgState Map.empty Map.empty Map.empty

newtype RuntimeError = RuntimeError String
  deriving Show

type ProgramM m = StateT ProgState (ExceptT RuntimeError m)

witnessSV 
  :: SomeStateVec
  -> (forall n. KnownNat n => StateVec n -> a)
  -> a
witnessSV ssv f = case ssv of
  SomeSV (sv :: StateVec n) -> f sv

-- | QReg's are often independant, so they can be backed by independant
-- StateVecs. But when QReg's become entangled we must store the combined
-- state. fuseQRegs forms the composite system and returns the Id of the
-- SomeStateVec stored in the ProgState, while updating QReg's to point to 
-- it. If both QReg's are already backed by the same StateVec, then NOP.
fuseQRegs :: Monad m => Id -> Id -> ProgramM m Id
fuseQRegs qr1 qr2 = do
  ps <- get
  let qrs = qregs ps
  (QReg ssvId1 i1 s1) <- findId qr1 qrs
  (QReg ssvId2 i2 s2) <- findId qr2 qrs
  if ssvId1 == ssvId2 
    then pure ssvId1
    else do 
      ssv1 <- findId ssvId1 (stVecs ps)
      ssv2 <- findId ssvId2 (stVecs ps)
      let ssvId' = ssvId1 ++ "(x)" ++ ssvId2
      -- Build the new StateVec
      witnessSV ssv1 $ \(sv1 :: StateVec n1) ->
        witnessSV ssv2 $ \(sv2 :: StateVec n2) ->
          let sv' = sv1 `tensor` sv2
          in  writeStateVec sv' ssvId'
      -- Update QReg's                  -- TODO: Check for additional QReg's
      writeQReg (QReg ssvId' i1 s1) qr1 -- which may refer to a removed 
      writeQReg (QReg ssvId' i2 s2) qr2 -- StateVec.
      -- Remove unused StateVecs
      deleteStateVec ssvId1
      deleteStateVec ssvId2
      pure ssvId'

findId :: Monad m => Id -> Map.Map Id v -> ProgramM m v
findId name table =
  case Map.lookup name table of
    Just v  -> pure v
    Nothing -> lift . throwE . RuntimeError
               $ "Undeclared identifier: " ++ name

addQReg :: Monad m => Id -> Size -> ProgramM m ()
addQReg name size = do
  ps <- get
  checkNameConflict name (qregs ps)
  let qr   = QReg name 0 size
      qrs' = Map.insert name qr (qregs ps)
  put $ ProgState (stVecs ps) qrs' (cregs ps)
  addStateVec name size

writeQReg :: Monad m => QReg -> Id -> ProgramM m ()
writeQReg qreg name = do
  ps <- get
  let qrs = qregs ps
  qr <- findId name qrs
  let qrs' = Map.insert name qreg qrs
  put $ ProgState (stVecs ps) qrs' (cregs ps)

addCReg :: Monad m => Id -> Size -> ProgramM m ()
addCReg name size = do
  ps <- get
  checkNameConflict name (cregs ps)
  let cr   = mkCReg $ replicate (fromIntegral size) Zero
      crs' = Map.insert name cr (cregs ps)
  put $ ProgState (stVecs ps) (qregs ps) crs'

writeCReg :: Monad m => CReg -> Id -> ProgramM m ()
writeCReg creg name = do
  ps <- get
  let crs = cregs ps
  cr <- findId name crs
  if crSize creg == crSize cr 
    then let crs' = Map.insert name creg crs
         in  put $ ProgState (stVecs ps) (qregs ps) crs'
    else lift . throwE . RuntimeError 
         $ "Mismatched size on overwrite of " ++ name

writeBit :: Monad m => Bit -> Id -> Index -> ProgramM m ()
writeBit bit name i = do
  ps <- get
  let crs = cregs ps
  cr <- findId name crs
  if i < crSize cr 
    then let crs' = Map.insert name (setBit i bit cr) crs
         in  put $ ProgState (stVecs ps) (qregs ps) crs'
    else lift . throwE . RuntimeError 
         $ "Index out of bounds when writing to " ++ name

addStateVec :: Monad m => Id -> Size -> ProgramM m ()
addStateVec name size = do
  ps <- get
  checkNameConflict name (stVecs ps)
  case someNatVal (toInteger size) of
    Just (SomeNat sn) -> do
      let sv   = SomeSV . mkStateVec' $ singByProxy sn
          svs' = Map.insert name sv (stVecs ps)
      put $ ProgState svs' (qregs ps) (cregs ps)
    Nothing -> undefined

writeStateVec :: (Monad m, KnownNat n) => StateVec n -> Id -> ProgramM m ()
writeStateVec sv name = do
  ps <- get
  let svs' = Map.insert name (SomeSV sv) (stVecs ps)
  put $ ProgState svs' (qregs ps) (cregs ps)

deleteStateVec :: Monad m => Id -> ProgramM m ()
deleteStateVec name = do
  ps <- get
  let svs = stVecs ps
  put $ ProgState (Map.delete name svs) (qregs ps) (cregs ps)

checkNameConflict :: Monad m => Id -> Map.Map Id v -> ProgramM m ()
checkNameConflict name table =
  if name `Map.member` table
    then lift . throwE . RuntimeError $ "Redeclaration of " ++ name
    else pure ()
