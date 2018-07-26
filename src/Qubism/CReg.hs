{-|
Module      : Qubism.CReg
Description : Types and functions for classical registers
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Qubism.CReg where

import           GHC.TypeLits
import           Data.Singletons
import           Data.Singletons.TypeLits
import qualified Data.Vector as V

data Bit = Zero | One
  deriving (Eq, Ord)

instance Show Bit where
  show One  = "1"
  show Zero = "0"

newtype CReg (n :: Nat) = CReg (V.Vector Bit)
  deriving Eq

mkCReg :: [Bit] -> CReg n
mkCReg = CReg . V.fromList

instance Show (CReg n) where
  show (CReg bs) = concatMap show bs
