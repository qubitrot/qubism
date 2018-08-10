{-|
Module      : Qubism.QASM.Simulation
Description : Simulation of an OpenQASM program
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
  show (ProgState sv qr cr) = "ProgState:\n" 
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
runStmt (QRegDecl name size) = do
  addQReg     name size
  addStateVec name size
runStmt (CRegDecl name size) =
  lift . throwE $ RuntimeError "not yet implimented"
runStmt (GateDecl name params args ops) = 
  lift . throwE $ RuntimeError "not yet implimented"
runStmt (QOp op) = case op of
  Measure arg1 arg2 -> lift . throwE $ RuntimeError "not yet implimented"
  Reset   arg       -> lift . throwE $ RuntimeError "not yet implimented"
runStmt (UOp op) = case op of
  U       p1 p2 p3 arg    -> unitary (expr p1) (expr p2) (expr p3) ##> arg
  CX      arg1 arg2       -> lift . throwE $ RuntimeError "not yet implimented"
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

-- | Apply a QGate to a QReg held in the ProgState
(##>) :: Monad m => QGate 1 -> Arg -> ProgramM m ()
(##>) g arg = do
  ps <- get
  (QReg idSV i s) <- getQReg (argId arg)
  case Map.lookup idSV (stVecs ps) of   -- There should be a cleaner way to
    Just (SomeSV (sv :: StateVec n)) -> -- witness these types. TODO.
      let ix j = finite $ toInteger (j+i) :: Finite n
          sv'  = case arg of 
                  (ArgQubit _ k) -> SomeSV $ onJust  (ix k)           g #> sv
                  (ArgReg   _  ) -> SomeSV $ onRange (ix 0) (ix(s-1)) g #> sv
          svs' = Map.insert idSV sv' (stVecs ps)
      in  put $ ProgState svs' (qregs ps) (cregs ps)
    Nothing -> pure ()

getQReg :: Monad m => Id -> ProgramM m QReg
getQReg name = do
  ps <- get
  case Map.lookup name (qregs ps) of
    Just qr -> pure qr
    Nothing -> lift . throwE . RuntimeError $ "Undeclared identifier: " ++ name 

addQReg :: Monad m => Id -> Natural -> ProgramM m ()
addQReg name size = do
  ps <- get
  checkNameConflict name (qregs ps)
  addStateVec name size
  let qrs  = qregs ps
      qr   = QReg name 0 size
      qrs' = Map.insert name qr qrs
  put $ ProgState (stVecs ps) qrs' (cregs ps)

getCReg :: Monad m => Arg -> ProgramM m QReg
getCReg arg = undefined
  

addStateVec :: Monad m => Id -> Natural -> ProgramM m ()
addStateVec name size = do
  ps <- get
  checkNameConflict name (stVecs ps)
  case someNatVal (toInteger size) of 
    Just (SomeNat sn) -> do
      let sv   = SomeSV . mkStateVec' $ singByProxy sn
          svs' = Map.insert name sv (stVecs ps)
      put $ ProgState svs' (qregs ps) (cregs ps)

checkNameConflict :: Monad m => Id -> Map.Map Id v -> ProgramM m ()
checkNameConflict name table = 
  if name `Map.member` table 
    then lift . throwE . RuntimeError $ "Redeclaration of " ++ name 
    else pure ()
