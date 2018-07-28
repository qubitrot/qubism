{-|
Module      : Qubism.Algebra
Description : Typeclasses and utilities for Vector and Hilbert spaces.
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

module Qubism.Algebra where

import Data.Complex
import Data.Monoid

type C = Complex Double

-- | A vector space over the complex numbers
class Eq v => VectorSpace v where
  zero :: v           -- | The additive identity
  (.:) :: C -> v -> v -- | Scalar multiplication
  (+:) :: v -> v -> v -- | Vector addition
  (-:) :: v -> v -> v -- | Vector subtraction
  neg  :: v -> v      -- | Inverse element
  a -: b = a +: (neg  b)

-- | A Hilbert space over the complex numbers
class VectorSpace h => HilbertSpace h where
  (<.>) :: h -> h -> C -- | Sesquilinear inner product
  norm  :: h -> Double -- | Norm induced by the inner product
  norm a = realPart $ a <.> a

class VectorSpace a => Algebra a where
  one  :: a           -- | Multiplicative identity
  (*:) :: a -> a -> a -- | Associative product that distributes 
                      --   over addition

commutator :: Algebra v  => v -> v -> v
commutator a b = (a *: b) -: (b *: a)

anticommutator :: Algebra v => v -> v -> v
anticommutator a b = (a *: b) +: (b *: a)
