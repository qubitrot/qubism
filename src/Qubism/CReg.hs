{-|
Module      : Qubism.CReg
Description : Types and functions for classical registers
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

module Qubism.CReg where

import qualified Data.Vector as V

data Bit = Zero | One
  deriving (Eq, Ord)

instance Show Bit where
  show One  = "1"
  show Zero = "0"

newtype CReg = CReg (V.Vector Bit)
  deriving Eq

instance Show CReg where
  show (CReg bs) = concatMap show bs

mkCReg :: [Bit] -> CReg
mkCReg = CReg . V.fromList

