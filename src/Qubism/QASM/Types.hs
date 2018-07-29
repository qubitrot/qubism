module Qubism.QASM.Types 
  ( module Qubism.QASM.Types
  , module Numeric.Natural
  ) where

import Numeric.Natural

type Ident   = String
type Size    = Natural
type Param   = Double
type Program = [Stmt]

data Stmt 
  = DeclReg Ident RegType Size
  | DeclGate Ident [Ident] [Ident] [Op]
  | Measure Arg Arg
  | Reset Arg
  | Operation Op
  deriving (Eq, Show)

data Op
  = Apply Gate [Arg]
  | Barrier [Arg]
  deriving (Eq, Show)

data RegType = CR | QR
  deriving (Eq, Show)

data Gate 
  = U Param Param Param
  | CX 
  | Custom Ident [Param]
  deriving (Eq, Show)

data Arg = Arg Ident (Maybe Natural)
  deriving (Eq, Show)
