{-|
Module      : Qubism.QASM.AST
Description : Defines an AST for OpenQASM 2.0
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

module Qubism.QASM.AST where

import Numeric.Natural
import Data.Text (Text)

type Id    = Text
type Size  = Natural
type Index = Natural
type AST   = [Stmt]

data Stmt
  = QRegDecl Id Size
  | CRegDecl Id Size
  | GateDecl 
      Id   -- ^ Name
      [Id] -- ^ Parameters
      [Id] -- ^ Arguments
      [UnitaryOp]
  | QOp QuantumOp
  | UOp UnitaryOp
  | Cond Id Natural QuantumOp
  deriving (Eq, Show)

data QuantumOp
  = QUnitary UnitaryOp
  | Measure  Arg Arg -- ^ source, target
  | Reset    Arg
  deriving (Eq, Show)

data UnitaryOp
  = U    Expr Expr Expr Arg
  | CX   Arg Arg
  | Func Id [Expr] [Arg]
  | Barrier [Arg] -- ^ Not a unitary op, but it's a convenient spot
  | Dump          -- ^ Not QASM official. Write internal state to console.
  deriving (Eq, Show)

data Arg
  = ArgBit Id Index
  | ArgReg Id
  deriving (Eq, Show)

argId :: Arg -> Id
argId (ArgBit name _) = name
argId (ArgReg name  ) = name

data Expr 
  = Pi
  | Ident Id 
  | Real Double
  | Binary BinaryOp Expr Expr
  | Unary  UnaryOp  Expr
  deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Div | Pow              deriving (Eq, Show)
data UnaryOp  = Neg | Sin | Cos | Tan | Exp | Ln | Sqrt  deriving (Eq, Show)

