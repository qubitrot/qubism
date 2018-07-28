{-# LANGUAGE ScopedTypeVariables #-}

module Qubism.AlgebraTests 
  ( module Test.QuickCheck.Property.Generic
  , isVectorSpace, isHilbertSpace, isAlgebra) 
  where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property.Generic
import Test.QuickCheck.Property.Common.Internal
import Data.Complex

import Qubism.Algebra

tupled :: T a -> T (a,a)
tupled _ = T

prop_Distributive 
  :: (a -> b -> b) -> (b -> b -> b) -> T a -> T b -> a -> b -> b -> Equal b
prop_Distributive f g _ _ a b c = Equal left right
  where left  = a `f` (b `g` c)
        right = (a `f` b) `g` (a `f` c)

isVectorSpace :: (Show v, Arbitrary v, VectorSpace v) => T v -> Spec
isVectorSpace t = do
  it "+: is associative"          $ property . eq $ prop_Associative (+:) t
  it "+: is commutative"          $ property . eq $ prop_Commutative (+:) (tupled t)
  it "zero is the identity of +:" $ property . eq $ prop_Identity zero (+:) t
  it "neg produces the inverse"   $ property $ 
    let test (_ :: T v) (v :: v) = neg v +: v == zero 
    in  test t
  it ".: distrbutes over +:" $ property . eq $ 
    prop_Distributive (.:) (+:) (T :: T C) t

prop_InnerLinear :: VectorSpace v => (v -> C) -> T v -> C -> v -> v -> Bool
prop_InnerLinear f _ a u v = magnitude (left - right) < 0.00001
  where left  = f $ (a .: u) +: v
        right = a * f u + f v

isHilbertSpace :: (Show v, Arbitrary v, HilbertSpace v) => T v -> Spec
isHilbertSpace t = do
  it "<.> is linear in the second argument" $ property $ 
    \w a u v -> prop_InnerLinear (w <.>) t a u v
  it "<a.b> = conj <b.a>" $ property $ 
    let test (_ :: T v)(a :: v)(b :: v) = a <.> b `shouldBe` conjugate (b <.> a)
    in  test t

prop_AlgLinear :: (Show v, Algebra v) => (v -> v) -> T v -> C -> v -> v -> Expectation
prop_AlgLinear f _ a u v = left `shouldBe` right
  where left  = f $ (a .: u) +: v
        right = (a .: f u) +: f v

isAlgebra :: (Show v, Arbitrary v, Algebra v) => T v -> Spec
isAlgebra t = do
  it "*: is linear in the first argument" $ property $ 
    \w a u v -> prop_AlgLinear (*: w) t a u v
  it "*: is linear in the second argument" $ property $
    \w a u v -> prop_AlgLinear (w *:) t a u v
