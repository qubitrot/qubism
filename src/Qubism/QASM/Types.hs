{-|
Module      : Qubism.QASM.Types
Description : Defines an AST for OpenQASM 2.0
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

module Qubism.QASM.Types where

import Numeric.Natural

type Id      = String
type Size    = Natural
type Index   = Natural
type Program = [Stmt]

data Stmt
  = StateVecDecl Id Size
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
  = Measure Arg Arg -- ^ source, target
  | Reset   Arg
  deriving (Eq, Show)

data UnitaryOp
  = U    [Expr] Arg
  | CX   Arg Arg
  | Func Id [Expr] [Arg]
  | Barrier [Arg] -- ^ Not a unitary op, but it's a convenient spot
  deriving (Eq, Show)

data Arg
  = ArgQubit Id Index
  | ArgReg   Id
  deriving (Eq, Show)

data Expr 
  = Pi
  | Ident Id 
  | Real Double
  | Binary BinaryOp Expr Expr
  | Unary  UnaryOp  Expr
  deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Div | Pow              deriving (Eq, Show)
data UnaryOp  = Neg | Sin | Cos | Tan | Exp | Ln | Sqrt  deriving (Eq, Show)

