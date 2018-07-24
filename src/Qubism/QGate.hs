{-|
Module      : Qubism.QGate
Description : Types and functions for quantum gates
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Qubism.QGate 
  ( QGate
  , (#>)
  , ident
  , pauliX
  , pauliY
  , pauliZ
  , hadamard
  ) where

-- For dependent typing
import GHC.TypeLits
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Finite

import           Data.Complex
import           Numeric.LinearAlgebra (C, (><))
import qualified Numeric.LinearAlgebra as LA

import Qubism.QReg

newtype QGate (n :: Nat) = 
  UnsafeMkQGate (LA.Matrix C) -- must be a (2^n) x (2^n) matrix
  deriving (Show, Eq)

-- | Apply a quantum gate to a quantum register. Note that this
-- operator conflicts with the one from Numeric.LinearAlgebra.
(#>) :: QGate n -> QReg n -> QReg n
(#>) (UnsafeMkQGate m) (UnsafeMkQReg v) = UnsafeMkQReg $ m LA.#> v

ident :: forall n . KnownNat n => QGate n
ident = UnsafeMkQGate $ LA.ident l
  where l = (2^) . fromIntegral $ fromSing (sing :: Sing n)

pauliX :: QGate 1
pauliX = UnsafeMkQGate $
  (2><2) [0 :+ 0, 1 :+ 0,
          1 :+ 0, 0 :+ 0]

pauliY :: QGate 1
pauliY = UnsafeMkQGate $
  (2><2) [0 :+ 0, 0 :+ (-1),
          0 :+ 1, 0 :+ 0]

pauliZ :: QGate 1
pauliZ = UnsafeMkQGate $
  (2><2) [1 :+ 0, 0 :+ 0,
          0 :+ 0, (-1) :+ 0]

hadamard :: QGate 1
hadamard = UnsafeMkQGate $ 1/(sqrt 2) *
  (2><2) [1 :+ 0, 1 :+ 0,
          1 :+ 0, (-1) :+ 0]
