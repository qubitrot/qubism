{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Qubism.QGateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Qubism.AlgebraTests
import Qubism.QGate

-- | I know, it's an orphan instance, but it's not relevant outside of this 
-- test suite, and never should be.
instance Arbitrary (QGate 1) where
  arbitrary = do
    theta  <- choose (0,4*pi)
    phi    <- choose (0,4*pi)
    lambda <- choose (0,4*pi)
    pure $ unitary theta phi lambda

spec :: Spec
spec = do
  describe "Quantum Gates" $ do
    describe "form a vector space" $ isVectorSpace (T :: T (QGate 1))
    describe "form an algebra"     $ isAlgebra     (T :: T (QGate 1))  


