{-|
Module      : Qubism.QASM.ProgState
Description : Internal state of an OpenQASM program
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Qubism.QASM.ProgState where

-- For dependent typing
import GHC.TypeLits
import Data.Singletons

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Random
import Numeric.Natural
import Text.Megaparsec (SourcePos(..), initialPos)
import Text.Megaparsec.Pos (unPos)

import Qubism.CReg
import Qubism.StateVec
import Qubism.QASM.AST

-- | This datatype encapsulates the concept of QReg's in QASM, which cannot
-- be considered independant. A QReg is a portion (perhaps the whole thing)
-- of a StateVec that is used in a calculation.
data QReg = QReg {
  qrTarget :: Id,      -- ^ Identifier of the StateVec holding the full state
  qrStart  :: Natural, -- ^ Index of the first qubit
  qrSize   :: Natural  -- ^ Size of the register
  } deriving Show

data SomeStateVec = forall n . KnownNat n => SomeSV (StateVec n)

instance Show SomeStateVec where
  show (SomeSV sv) = "\n" ++ show sv

witnessSV 
  :: SomeStateVec
  -> (forall n. KnownNat n => StateVec n -> a)
  -> a
witnessSV ssv f = case ssv of
  SomeSV (sv :: StateVec n) -> f sv

data CustomGate = CustomGate [Id] [Id] [UnitaryOp]
  deriving Show

data ProgState = ProgState
  { stVecs :: Map Id SomeStateVec
  , qregs  :: Map Id QReg
  , cregs  :: Map Id CReg
  , funcs  :: Map Id CustomGate
  , pos    :: SourcePos
  }

instance Show ProgState where
  show (ProgState sv qr cr _ p) = "ProgState_________________\n"
    ++ "StateVecs: " ++ show sv ++ "\n"
    ++ "QRegs:     " ++ show qr ++ "\n"
    ++ "CRegs:     " ++ show cr ++ "\n"
--  ++ "Funcs:     " ++ show fs ++ "\n"
    ++ "SourcePos: " ++ show p  ++ "\n"

blankState :: ProgState
blankState = ProgState Map.empty Map.empty 
                       Map.empty Map.empty (initialPos "") --Could be better

data RuntimeError = RuntimeError SourcePos T.Text

instance Show RuntimeError where
  show (RuntimeError pos msg) =
    let (SourcePos f l _) = pos
        line = show $ unPos l
    in "ERROR on line " ++ line ++ " in " ++ f ++ "\n" ++ T.unpack msg

type ProgramM m = StateT ProgState (ExceptT RuntimeError m)

runtimeE :: Monad m => T.Text -> ProgramM m a
runtimeE msg = do
  pos <- gets pos
  lift . throwE $ RuntimeError pos msg

putPos :: Monad m => SourcePos -> ProgramM m ()
putPos p = get >>= \ps -> put $
  ProgState (stVecs ps) (qregs ps) (cregs ps) (funcs ps) p

putSVecs :: Monad m => Map Id SomeStateVec -> ProgramM m ()
putSVecs ssv = get >>= \ps -> put $
  ProgState ssv (qregs ps) (cregs ps) (funcs ps) (pos ps)

putQRegs :: Monad m => Map Id QReg -> ProgramM m ()
putQRegs qrs = get >>= \ps -> put $
  ProgState (stVecs ps) qrs (cregs ps) (funcs ps) (pos ps)

putCRegs :: Monad m => Map Id CReg -> ProgramM m ()
putCRegs crs = get >>= \ps -> put $
  ProgState (stVecs ps) (qregs ps) crs (funcs ps) (pos ps)

putFuncs :: Monad m => Map Id CustomGate -> ProgramM m ()
putFuncs fs = get >>= \ps -> put $
  ProgState (stVecs ps) (qregs ps) (cregs ps) fs (pos ps)

-- | QReg's are often independant, so they can be backed by independant
-- StateVecs. But when QReg's become entangled we must store the combined
-- state. fuseQRegs forms the composite system and returns the Id of the
-- SomeStateVec stored in the ProgState, while updating QReg's to point to 
-- it. If both QReg's are already backed by the same StateVec, then NOP.
fuseQRegs :: Monad m => Id -> Id -> ProgramM m Id
fuseQRegs qr1 qr2 = do
  ps <- get
  let qrs = qregs ps
  (QReg ssvId1 _ _) <- findId qr1 qrs
  (QReg ssvId2 _ _) <- findId qr2 qrs
  if ssvId1 == ssvId2 
    then pure ssvId1
    else do 
      ssv1 <- findId ssvId1 (stVecs ps)
      ssv2 <- findId ssvId2 (stVecs ps)
      let ssvId' = ssvId1 <> "(x)" <> ssvId2
      -- Build the new StateVec
      witnessSV ssv1 $ \(sv1 :: StateVec n1) ->
        witnessSV ssv2 $ \(sv2 :: StateVec n2) -> do
          let sv' = sv1 `tensor` sv2
          writeStateVec sv' ssvId'
      -- Update QReg's
          let shift = dimension sv1
              pass1 = retargetQReg ssvId1 ssvId' 0     <$> qrs
              pass2 = retargetQReg ssvId2 ssvId' shift <$> pass1
          putQRegs pass2
      -- Remove unused StateVecs
      deleteStateVec ssvId1
      deleteStateVec ssvId2
      pure ssvId'
  where
    retargetQReg v v' shift (QReg s i1 s1)
      | v == s    = QReg v' (i1+shift) s1
      | otherwise = QReg s i1 s1

findId :: Monad m => Id -> Map.Map Id v -> ProgramM m v
findId name table =
  case Map.lookup name table of
    Just v  -> pure v
    Nothing -> runtimeE $ "Undeclared identifier: " <> name

addQReg :: Monad m => Id -> Size -> ProgramM m ()
addQReg name size = do
  ps <- get
  checkNameConflict name (qregs ps)
  let qr   = QReg name 0 size
      qrs' = Map.insert name qr (qregs ps)
  putQRegs qrs'
  addStateVec name size

writeQReg :: Monad m => QReg -> Id -> ProgramM m ()
writeQReg qreg name = do
  ps <- get
  let qrs = qregs ps
  _  <- findId name qrs --Just to ensure it exists
  let qrs' = Map.insert name qreg qrs
  putQRegs qrs'

addCReg :: Monad m => Id -> Size -> ProgramM m ()
addCReg name size = do
  ps <- get
  checkNameConflict name (cregs ps)
  let cr   = mkCReg $ replicate (fromIntegral size) Zero
      crs' = Map.insert name cr (cregs ps)
  putCRegs crs'

writeCReg :: Monad m => CReg -> Id -> ProgramM m ()
writeCReg creg name = do
  ps <- get
  let crs = cregs ps
  cr <- findId name crs
  if crSize creg == crSize cr 
    then let crs' = Map.insert name creg crs
         in  putCRegs crs'
    else runtimeE $ "Mismatched size on overwrite of " <> name

writeBit :: Monad m => Bit -> Id -> Index -> ProgramM m ()
writeBit bit name i = do
  ps <- get
  let crs = cregs ps
  cr <- findId name crs
  if i < crSize cr 
    then let crs' = Map.insert name (setBit i bit cr) crs
         in  putCRegs crs'
    else runtimeE $ "Index out of bounds when writing to " <> name

addStateVec :: Monad m => Id -> Size -> ProgramM m ()
addStateVec name size = do
  ps <- get
  checkNameConflict name (stVecs ps)
  case someNatVal (toInteger size) of
    Just (SomeNat sn) -> do
      let sv   = SomeSV . mkStateVec' $ singByProxy sn
          svs' = Map.insert name sv (stVecs ps)
      putSVecs svs'
    Nothing -> undefined

writeStateVec :: (Monad m, KnownNat n) => StateVec n -> Id -> ProgramM m ()
writeStateVec sv name = do
  ps <- get
  let svs' = Map.insert name (SomeSV sv) (stVecs ps)
  putSVecs svs'

deleteStateVec :: Monad m => Id -> ProgramM m ()
deleteStateVec name = do
  ps <- get
  let svs = stVecs ps
  putSVecs $ Map.delete name svs

addFunc :: Monad m => CustomGate -> Id -> ProgramM m ()
addFunc cg name = do
  ps <- get
  let funcs' = Map.insert name cg (funcs ps)
  putFuncs funcs'

checkNameConflict :: Monad m => Id -> Map.Map Id v -> ProgramM m ()
checkNameConflict name table =
  if name `Map.member` table
    then runtimeE $ "Redeclaration of " <> name
    else pure ()

findQRSize :: Monad m => Id -> ProgramM m Natural
findQRSize qr = do
  qrs <- gets qregs
  (QReg _ _ s) <- findId qr qrs
  pure s
