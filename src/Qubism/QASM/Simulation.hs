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
import Data.Singletons.TypeLits
import Data.Finite

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Random
import Numeric.Natural

import Qubism.CReg
import Qubism.StateVec
import Qubism.QGate
import Qubism.QASM.AST
import Qubism.QASM.ProgState

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
  QUnitary op        -> runStmt $ UOp op
  Measure  argQ argC -> observe argQ argC
  Reset    arg       -> lift . throwE $ RuntimeError "not yet implimented"
runStmt (UOp op) = case op of
  U       p1 p2 p3 arg    -> unitary (expr p1) (expr p2) (expr p3) ##> arg
  CX      arg1 arg2       -> cx arg1 arg2
  Func    name exprs args -> lift . throwE $ RuntimeError "not yet implimented"
  Barrier args            -> lift . throwE $ RuntimeError "not yet implimented"
runStmt (Cond name nat op) = do
  ps <- get
  cr <- findId name (cregs ps)
  when (crToNatural cr == nat) $ runStmt (QOp op)

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
