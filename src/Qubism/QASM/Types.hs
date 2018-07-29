module Qubism.QASM.Types 
  ( module Qubism.QASM.Types
  , module Numeric.Natural
  ) where

import Numeric.Natural

type Ident   = String
type Size    = Natural
type Index   = Natural
type Program = [Stmt]

newtype RegIdent   = RegIdent   Ident  deriving (Eq, Show)
newtype GateIdent  = GateIdent  Ident  deriving (Eq, Show)
newtype ParamIdent = ParamIdent Ident  deriving (Eq, Show)
newtype ArgIdent   = ArgIdent   Ident  deriving (Eq, Show)

data Stmt 
  = RegDef  RegIdent RegType Size
  | GateDef GateIdent [ParamIdent] [ArgIdent] [GateBody]
  | Measure Reg Reg
  | Reset   Reg
  | Barrier [Reg]
  | Apply   Gate [RegIdent]
  deriving (Eq, Show)

data Reg = Reg RegIdent (Maybe Index)
  deriving (Eq, Show)

data RegType = CR | QR
  deriving (Eq, Show)

data GateBody 
  = GBApply Gate [ArgIdent]
  | GBBarrier [ArgIdent]
  deriving (Eq, Show)

data Gate 
  = CX
  | U Param Param Param
  | Func GateIdent [Param]
  deriving (Eq, Show)

data Param 
  = Number Double 
  | Name ParamIdent
  deriving (Eq, Show)


