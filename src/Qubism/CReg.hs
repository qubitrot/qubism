{-|
Module      : Qubism.CReg
Description : Types and functions for classical registers
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

module Qubism.CReg where

import qualified Data.Vector as V
import           Numeric.Natural

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

cregBit :: Bit -> CReg
cregBit b = CReg $ V.singleton b

crSize :: CReg -> Natural
crSize (CReg v) = fromIntegral $ V.length v

crToNatural :: CReg -> Natural
crToNatural (CReg v) = sum . fmap f $ V.indexed v
  where f (i, b) | b == One  = 2^i
                 | otherwise = 0;

-- | Non-total. Be careful with indexing.
setBit :: Natural -> Bit -> CReg -> CReg
setBit i bit (CReg bs) = CReg $ bs V.// [(j,bit)]
  where j = fromIntegral $ toInteger i

-- | Non-total. Be careful with indexing.
crIndex :: CReg -> Natural -> Bit
crIndex (CReg bs) i = bs V.! j
  where j = fromIntegral $ toInteger i
