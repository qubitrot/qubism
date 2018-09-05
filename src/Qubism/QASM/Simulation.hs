{-|
Module      : Qubism.QASM.Simulation
Description : Simulation of an OpenQASM program
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Qubism.QASM.Simulation where

-- For dependent typing
import GHC.TypeLits
import Data.Singletons
import Data.Finite

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Random
import Numeric.Natural

import Qubism.CReg
import Qubism.StateVec
import Qubism.QGate
import Qubism.QASM.Types

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

data RuntimeError = RuntimeError String
  deriving Show

type ProgramM m = StateT ProgState (ExceptT RuntimeError m)

runProgram :: Program -> IO (Either RuntimeError ProgState)
runProgram prog =
  let comp = mapM_ runStmt prog
  in  runExceptT . execStateT comp $ blankState

runStmt :: MonadRandom m => Stmt -> ProgramM m ()
runStmt (QRegDecl name size) = addQReg name size
runStmt (CRegDecl name size) = addCReg name size
runStmt (GateDecl name params args ops) =
  lift . throwE $ RuntimeError "not yet implimented"
runStmt (QOp op) = case op of
  Measure argQ argC -> observe argQ argC
  Reset   arg       -> lift . throwE $ RuntimeError "not yet implimented"
runStmt (UOp op) = case op of
  U       p1 p2 p3 arg    -> unitary (expr p1) (expr p2) (expr p3) ##> arg
  CX      arg1 arg2       -> cx arg1 arg2
  Func    name exprs args -> lift . throwE $ RuntimeError "not yet implimented"
  Barrier args            -> lift . throwE $ RuntimeError "not yet implimented"
runStmt (Cond name nat op) =
  lift . throwE $ RuntimeError "not yet implimented"

expr :: Expr -> Double
expr e = case e of
  Pi       -> 3.14159265358979
  Ident  _ -> undefined
  Real   a -> a
  Binary op a b -> case op of
    Add -> expr a +  expr b
    Sub -> expr a -  expr b
    Mul -> expr a *  expr b
    Div -> expr a /  expr b
    Pow -> expr a ** expr b
  Unary op a -> case op of
    Neg  -> - expr a
    Sin  -> sin  $ expr a
    Cos  -> cos  $ expr a
    Tan  -> tan  $ expr a
    Exp  -> exp  $ expr a
    Ln   -> log  $ expr a
    Sqrt -> sqrt $ expr a

witnessSV 
  :: SomeStateVec
  -> (forall n. KnownNat n => StateVec n -> a)
  -> a
witnessSV ssv f = case ssv of
  SomeSV (sv :: StateVec n) -> f sv

-- | Apply a single qubit gate with an argument
(##>) :: Monad m => QGate 1 -> Arg -> ProgramM m ()
(##>) g arg = do
  ps <- get
  (QReg idSV i s) <- findId (argId arg) (qregs ps)
  ssv             <- findId idSV (stVecs ps)
  witnessSV ssv $ \sv ->
    let ix j = finite $ toInteger (j+i)
        sv'  = case arg of
          (ArgBit _ k) -> onJust  (ix k)           g #> sv
          (ArgReg _  ) -> onRange (ix 0) (ix(s-1)) g #> sv
    in  writeStateVec sv' idSV

applyOn 
  :: Monad m 
  => (forall n. KnownNat n => Finite n -> QGate n) 
  -> Arg -> ProgramM m ()
applyOn g arg = do
  ps <- get
  (QReg idSV i s) <- findId (argId arg) (qregs ps)
  ssv             <- findId idSV (stVecs ps)
  witnessSV ssv $ \sv ->
    let ix j = finite $ toInteger (j+i)
        sv'  = case arg of
          (ArgBit _ k) -> g (ix k) #> sv
          (ArgReg _  ) -> undefined
    in  writeStateVec sv' idSV

-- | Lift a stateful, indexed, single qubit function into the ProgramM
-- context. Basically only used as a helper function for measurement atm.
applyTo
  :: Monad m
  => (forall n. KnownNat n 
      => Finite n 
      -> StateT (StateVec n) m a)
  -> Natural
  -> Arg
  -> ProgramM m a
applyTo st i arg = do
  ps <- get
  (QReg idSV j _) <- findId (argId arg) (qregs ps)
  ssv             <- findId idSV (stVecs ps)
  witnessSV ssv $ \sv -> do
    let k = finite $ toInteger (i+j)
    (a, sv') <- lift . lift $ runStateT (st k) sv
    writeStateVec sv' idSV
    pure a

observe :: MonadRandom m => Arg -> Arg -> ProgramM m ()
observe argQ argC = do
  bits <- case argQ of 
    ArgBit _ k  -> cregBit <$> applyTo measureQubit k argQ
    ArgReg name -> do
      ps <- get
      (QReg _ i s) <- findId name (qregs ps)
      let mq j = applyTo measureQubit j argQ
      mkCReg <$> traverse mq [i..i+s-1]
  case argC of
    ArgBit name k -> writeBit (crIndex bits 0) name k
    ArgReg name   -> writeCReg bits (argId argC)

cx :: Monad m => Arg -> Arg -> ProgramM m ()
cx arg1 arg2 = do
  ps <- get
  let qr1 = argId arg1
      qr2 = argId arg2
  idSV <- fuseQRegs qr1 qr2 -- NOP if qr1 and qr2 are supported by the 
  (QReg _ i s) <- findId qr1 (qregs ps) -- same underlying StateVec.
  case arg1 of 
    (ArgBit _ k) -> 
      cnot (finite $ toInteger (i + k)) `applyOn` arg2 
    (ArgReg _  ) -> 
      let cn k = cnot (finite $ toInteger (i + k)) `applyOn` arg2
      in  mapM_ cn [i..i+s-1]

fuseQRegs :: Monad m => Id -> Id -> ProgramM m Id
fuseQRegs qr1 qr2 = do
  ps <- get
  (QReg ssv1 i1 s1) <- findId qr1 (qregs ps)
  (QReg ssv2 i2 s2) <- findId qr2 (qregs ps)
  if ssv1 == ssv2 
    then pure ssv1
    else undefined

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

checkNameConflict :: Monad m => Id -> Map.Map Id v -> ProgramM m ()
checkNameConflict name table =
  if name `Map.member` table
    then lift . throwE . RuntimeError $ "Redeclaration of " ++ name
    else pure ()
  
