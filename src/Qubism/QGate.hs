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
  , kronecker
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
  UnsafeMkQGate (LA.Matrix C) -- must be a (2^n) x (2^n) unitary matrix
  deriving (Show, Eq)

instance KnownNat n => Monoid (QGate n) where
  mempty = ident
  mappend (UnsafeMkQGate a) (UnsafeMkQGate b) = UnsafeMkQGate $ a LA.<> b

-- | Apply a quantum gate to a quantum register. Note that this
-- operator conflicts with the one from Numeric.LinearAlgebra.
infixr 5 #>
(#>) :: QGate n -> QReg n -> QReg n
(#>) (UnsafeMkQGate m) (UnsafeMkQReg v) = UnsafeMkQReg $ m LA.#> v

ident :: forall n . KnownNat n => QGate n
ident = UnsafeMkQGate $ LA.ident l
  where l = (2^) . fromIntegral $ fromSing (sing :: Sing n)

-- | Also known as a NOT gate
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

-- | The tensor product of a QGate's A and B is a QGate that acts as A on the 
-- first n qubits and B on the rest. In the computational basis this is simply
-- the kronecker product of matricies.
kronecker :: QGate n -> QGate m -> QGate (m+n)
kronecker (UnsafeMkQGate a) (UnsafeMkQGate b) =
  UnsafeMkQGate $ LA.kronecker a b

-- | Promote a 1-qubit gate to an n-qubit gate with the original gate acting 
-- on qubit i. Other qubits are unaffected.
promote :: forall n . KnownNat n => QGate 1 -> Finite n  -> QGate n
promote (UnsafeMkQGate m) i = UnsafeMkQGate $ -- it should be possible to do
  pre `LA.kronecker` m `LA.kronecker` post    -- this with just QGate's 
  where pre  = LA.ident $ 2^j                 -- kronkecer, but I can't get
        post = LA.ident $ 2^(n-j-1)           -- the types to check.
        j    = fromIntegral $ getFinite i
        n    = fromIntegral $ fromSing (sing :: Sing n)
