{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qubism.StateVecSpec (genStateVec, spec) where

import GHC.TypeLits
import Data.Singletons

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Control.Monad.Random
import           Control.Monad.Trans.State
import qualified Numeric.LinearAlgebra as LA
import           Data.Complex

import Qubism.AlgebraTests
import Qubism.StateVec

genC :: Gen (Complex Double)
genC = do
  a <- choose (-1, 1)
  b <- choose (-1, 1)
  pure $ a :+ b

genStateVec :: forall n . KnownNat n => Gen (StateVec n)
genStateVec = normalize . UnsafeMkStateVec . LA.fromList <$> vectorOf l genC
  where l = (2 ^) . fromIntegral . fromSing $ (sing :: Sing n)

-- | I know, it's an orphan instance, but it's not relevant outside of this 
-- test suite, and never should be.
instance KnownNat n => Arbitrary (StateVec n) where
  arbitrary = genStateVec

propIdempotent
  :: (Eq a, Show a, Eq b, Show b)
  => StateT a (Rand StdGen) b
  -> a
  -> PropertyM IO Expectation
propIdempotent st a = do
  g <- lift getStdGen
  let one = runStateT st a `evalRand` g
      two = runStateT (st >> st) a `evalRand` g
  pure $ two `shouldBe` one

spec :: Spec
spec = do
  describe "Quantum Registers" $ do
    describe "form a vector space"  $ isVectorSpace  (T :: T (StateVec 1))
    describe "form a hilbert space" $ isHilbertSpace (T :: T (StateVec 1))
    describe "measure" $ 
      it "is idempotent"
        $ property
        $ monadicIO
        $ forAllM (genStateVec :: Gen (StateVec 1)) 
        $ propIdempotent measure
    describe "measureQubit" $ 
      it "is idempotent"
        $ property
        $ monadicIO
        $ forAllM (genStateVec :: Gen (StateVec 1))
        $ propIdempotent (measureQubit 0)
