{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qubism.QRegSpec (genQReg, spec) where

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
import Qubism.QReg

genC :: Gen (Complex Double)
genC = do
  a <- choose (-1, 1)
  b <- choose (-1, 1)
  pure $ a :+ b

genQReg :: forall n . KnownNat n => Gen (QReg n)
genQReg = normalize . UnsafeMkQReg . LA.fromList <$> vectorOf l genC
  where l = (2 ^) . fromIntegral . fromSing $ (sing :: Sing n)

-- | I know, it's an orphan instance, but it's not relevant outside of this 
-- test suite, and never should be.
instance KnownNat n => Arbitrary (QReg n) where
  arbitrary = genQReg

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
    describe "form a vector space"  $ isVectorSpace  (T :: T (QReg 1))
    describe "form a hilbert space" $ isHilbertSpace (T :: T (QReg 1))
    describe "measure" $ 
      it "is idempotent"
        $ property
        $ monadicIO
        $ forAllM (genQReg :: Gen (QReg 1)) 
        $ propIdempotent measure
    describe "measureQubit" $ 
      it "is idempotent"
        $ property
        $ monadicIO
        $ forAllM (genQReg :: Gen (QReg 1))
        $ propIdempotent (measureQubit 0)
