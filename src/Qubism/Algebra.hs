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
  -- | The additive identity
  zero :: v           
  -- | Scalar multiplication
  (.:) :: C -> v -> v 
  -- | Vector addition
  (+:) :: v -> v -> v 
  -- | Vector subtraction 
  (-:) :: v -> v -> v
  -- | Inverse element
  neg  :: v -> v      
  a -: b = a +: (neg  b)

-- | A Hilbert space over the complex numbers
class VectorSpace v => HilbertSpace v where
  -- | Sesquilinear inner product
  (<.>) :: v -> v -> C 
  -- | Norm induced by the inner product
  norm  :: v -> Double 
  norm a = realPart $ a <.> a

class VectorSpace v => Algebra v where
  -- | Bilinear product 
  (*:) :: v -> v -> v 

commutator :: Algebra v  => v -> v -> v
commutator a b = (a *: b) -: (b *: a)

anticommutator :: Algebra v => v -> v -> v
anticommutator a b = (a *: b) +: (b *: a)
