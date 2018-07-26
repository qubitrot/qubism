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

import           Qubism.QReg

genC :: Gen (Complex Double)
genC = do
  a <- choose (-1, 1)
  b <- choose (-1, 1)
  pure $ a :+ b

genQReg :: forall n . KnownNat n => Gen (QReg n)
genQReg = normalize . UnsafeMkQReg . LA.fromList <$> vectorOf l genC
  where l = (2 ^) . fromIntegral . fromSing $ (sing :: Sing n)

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
  describe "measure" $ 
    it "is idempotent"
      $ property
      $ monadicIO
      $ forAllM (genQReg :: Gen (QReg 1)) --TODO tests for larger QRegs
      $ propIdempotent measure
  describe "measureQubit" $ 
    it "is idempotent"
      $ property
      $ monadicIO
      $ forAllM (genQReg :: Gen (QReg 1))
      $ propIdempotent (measureQubit 0)
