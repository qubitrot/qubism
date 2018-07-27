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
  , unitary
  , kronecker
  , onJust
  , onEvery
  ) where

-- For dependent typing
import GHC.TypeLits
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Finite

import           Data.Complex
import           Data.Monoid
import           Numeric.LinearAlgebra ((><))
import qualified Numeric.LinearAlgebra as LA

import Qubism.Algebra
import Qubism.QReg

newtype QGate (n :: Nat) = 
  UnsafeMkQGate (LA.Matrix C) -- must be a (2^n) x (2^n) unitary matrix
  deriving (Show, Eq)

instance KnownNat n => Monoid (QGate n) where
  mempty = ident
  mappend (UnsafeMkQGate a) (UnsafeMkQGate b) = UnsafeMkQGate $ a LA.<> b

instance KnownNat n => VectorSpace (QGate n) where
  zero = UnsafeMkQGate . (l><l) $ repeat 0 where l = internalLen (sing :: Sing n)
  z                 .: (UnsafeMkQGate b) = UnsafeMkQGate $ LA.scalar z * b
  (UnsafeMkQGate a) +: (UnsafeMkQGate b) = UnsafeMkQGate $ a + b
  (UnsafeMkQGate a) -: (UnsafeMkQGate b) = UnsafeMkQGate $ a - b

instance KnownNat n => Algebra (QGate n) where
  one  = ident
  (*:) = (<>)

internalLen :: (KnownNat n, Num a) => Sing n -> a
internalLen = (2 ^) . fromIntegral . fromSing 

-- | Apply a quantum gate to a quantum register. Note that this
-- operator conflicts with the one from Numeric.LinearAlgebra.
infixr 5 #>
(#>) :: QGate n -> QReg n -> QReg n
(#>) (UnsafeMkQGate m) (UnsafeMkQReg v) = UnsafeMkQReg $ m LA.#> v

ident :: forall n . KnownNat n => QGate n
ident = UnsafeMkQGate . LA.ident $ internalLen (sing :: Sing n)

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
hadamard = UnsafeMkQGate $ 1 / sqrt 2 *
  (2><2) [1 :+ 0, 1 :+ 0,
          1 :+ 0, (-1) :+ 0]

-- | An arbitrary element of SU(2),
-- U(theta, phi, lambda)
unitary :: Double -> Double -> Double -> QGate 1
unitary theta phi lambda = UnsafeMkQGate $ 
  (2><2) [a, b, c, d]
  where a =  cis (phi+lambda/2) * ( cos (theta/2) :+ 0 )
        b = -cis (phi-lambda/2) * ( sin (theta/2) :+ 0 )
        c =  cis (phi-lambda/2) * ( sin (theta/2) :+ 0 )
        d =  cis (phi+lambda/2) * ( cos (theta/2) :+ 0 )

-- | The tensor product of a QGate's A and B is a QGate that acts as A on the 
-- first n qubits and B on the rest. In the computational basis this is simply
-- the kronecker product of matricies.
kronecker :: QGate n -> QGate m -> QGate (m+n)
kronecker (UnsafeMkQGate a) (UnsafeMkQGate b) =
  UnsafeMkQGate $ LA.kronecker a b

-- | Promote a 1-qubit gate to an n-qubit gate with the original gate acting 
-- on qubit i. Other qubits are unaffected.
onJust :: forall n . KnownNat n => QGate 1 -> Finite n  -> QGate n
onJust (UnsafeMkQGate m) i = UnsafeMkQGate $  -- it should be possible to do
  pre `LA.kronecker` m `LA.kronecker` post    -- this with just QGate's 
  where pre  = LA.ident $ 2^j                 -- kronecker, but I can't get
        post = LA.ident $ 2^(n-j-1)           -- the types to check.
        j    = fromIntegral $ getFinite i
        n    = fromIntegral $ fromSing (sing :: Sing n)

-- | Promote a 1-qubit gate to an n-qubit gate which acts on every qubit
-- identically.
onEvery :: forall n . KnownNat n => QGate 1 -> QGate n
onEvery (UnsafeMkQGate m) = UnsafeMkQGate $ iterate (LA.kronecker m) m !! (n-1)
  where n = fromIntegral $ fromSing (sing :: Sing n)
