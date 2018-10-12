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
{-# LANGUAGE LambdaCase          #-}

module Qubism.QASM.Simulation 
  ( runProgram
  , runStmt
  ) where

-- For dependent typing
import GHC.TypeLits
import Data.Singletons
import Data.Finite

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
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
  let cg = CustomGate params args ops
  in  addFunc cg name
runStmt (QOp op) = case op of
  QUnitary uop       -> runStmt $ UOp uop
  Measure  argQ argC -> observe argQ argC
  Reset    arg       -> runtimeE "not yet implemented"
runStmt (UOp op) = case op of
  U       p1 p2 p3 arg    -> unitary (expr p1) (expr p2) (expr p3) ##> arg
  CX      arg1 arg2       -> cx arg1 arg2
  Func    name exprs args -> customOp name (expr <$> exprs) args
  Barrier _               -> pure ()
runStmt (Cond name nat op) = do
  ps <- get
  cr <- findId name (cregs ps)
  when (crToNatural cr == nat) $ runStmt (QOp op)

-- | Apply a single qubit gate with an argument
(##>) :: Monad m => QGate 1 -> Arg -> ProgramM m ()
(##>) g arg = case arg of
  (ArgBit qr k) -> withIndex  (`onJust` g) qr k
  (ArgReg qr  ) -> do
    qrs <- gets qregs
    (QReg _ _ s) <- findId qr qrs
    withIndex2 (\i j -> onRange i j g) qr 0 qr (s-1)

-- | Apply an index-dependant gate to a QReg
withIndex 
  :: Monad m 
  => (forall n. KnownNat n => Finite n -> QGate n) 
  -> Id -- ^ QReg identifier
  -> Index
  -> ProgramM m ()
withIndex g qr i = do
  ps              <- get
  (QReg idSV j _) <- findId qr   (qregs ps)
  ssv             <- findId idSV (stVecs ps)
  witnessSV ssv $ \sv ->
    let idx = finite . toInteger $ j+i
        sv' = g idx #> sv
    in  writeStateVec sv' qr

-- | Apply an gate dependant on 2 indicies, potentially across differet QRegs.
withIndex2
  :: Monad m 
  => (forall n. KnownNat n => Finite n -> Finite n -> QGate n) 
  -> Id    -- ^ QReg 1 identifier
  -> Index -- ^ QReg 1 index
  -> Id    -- ^ QReg 2 identifier
  -> Index -- ^ QReg 2 index
  -> ProgramM m ()
withIndex2 g qr1 i qr2 j = do
  ps   <- get
  idSV <- fuseQRegs qr1 qr2       -- We must fuse because this is potentially
  ssv  <- findId idSV (stVecs ps) -- an entangling operation.
  (QReg _ s1 _) <- findId qr1 (qregs ps)
  (QReg _ s2 _) <- findId qr2 (qregs ps)
  witnessSV ssv $ \sv ->
    let idx1 = finite . toInteger $ s1+i
        idx2 = finite . toInteger $ s2+j
        sv'  = g idx1 idx2 #> sv
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
    ArgReg _      -> writeCReg bits (argId argC)

cx :: Monad m => Arg -> Arg -> ProgramM m ()
cx arg1 arg2 = 
  let over qr f = findQRSize qr >>= \s -> mapM_ f [0..(s-1)] 
  in case arg1 of
    ArgBit qr1 i -> case arg2 of
      ArgBit qr2 j -> withIndex2 cnot qr1 i qr2 j
      ArgReg qr2   -> over qr2 $ withIndex2 cnot qr1 i qr2
    ArgReg qr1   -> case arg2 of 
      ArgBit qr2 j -> over qr1 $ \i -> withIndex2 cnot qr1 i qr2 j
      ArgReg qr2   -> do
        s1 <- findQRSize qr1
        s2 <- findQRSize qr2
        if s1 == s2 
          then mapM_ (\i -> withIndex2 cnot qr1 i qr2 i) [0..(s1-1)]
          else runtimeE $ "QRegs of different sizes supplied to CX: "
                       ++ qr1 ++ " " ++ qr2;

customOp :: MonadRandom m => Id -> [Double] -> [Arg] -> ProgramM m ()
customOp name params args = do
  progSt <- get
  (CustomGate ps as uops) <- findId name (funcs progSt)
  let argBinds   = Map.fromList $ zip as args
      paramBinds = Map.fromList $ zip ps params
  uop <- traverse (bindNames paramBinds argBinds) uops
  mapM_ runStmt (UOp <$> uop)

bindNames :: Monad m 
  => Map Id Double 
  -> Map Id Arg 
  -> UnitaryOp 
  -> ProgramM m UnitaryOp
bindNames etable atable op = case op of
  U  a b c d -> liftM4 U  (bindE a) (bindE b) (bindE c) (bindA d)
  CX a b     -> liftM2 CX (bindA a) (bindA b)
  Barrier as -> Barrier <$> traverse bindA as
  Func i e a -> liftM2 (Func i) (traverse bindE e) (traverse bindA a)
  where
    bindE = \case 
      (Binary o a b) -> liftM2 (Binary o) (bindE a) (bindE b)
      (Unary  o a  ) -> fmap   (Unary  o) (bindE a)
      (Ident  i    ) -> case Map.lookup i etable of
                          Just a  -> pure $ Real a
                          Nothing -> runtimeE $ "Could not bind " ++ i
      other          -> pure other
    bindA = \case
      (ArgBit _ _  ) -> runtimeE "Attempted to bind an ArgBit"
      (ArgReg i    ) -> case Map.lookup i atable of
                          Just a  -> pure a
                          Nothing -> runtimeE $ "Could not bind " ++ i

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
