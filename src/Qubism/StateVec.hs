{-|
Module      : Qubism.StateVec
Description : Types and functions for quantum state vectors
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Qubism.StateVec
  ( StateVec (UnsafeMkStateVec)
  , mkStateVec
  , mkStateVec'
  , mkQubit
  , normalize
  , tensor
  , measureQubit
  , measure
  ) where

-- For dependent typing
import GHC.TypeLits
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Finite

import           Control.Monad.Random
import           Control.Monad.Trans.State
import           Data.Complex
import           Numeric.LinearAlgebra ((|>))
import qualified Numeric.LinearAlgebra as LA

import Qubism.Algebra
import Qubism.CReg

newtype StateVec (n :: Nat) =
  UnsafeMkStateVec (LA.Vector C)

-- | To provide "close enough" equality testing.
instance Eq (StateVec n) where
  UnsafeMkStateVec zs == UnsafeMkStateVec ws = 
     LA.norm_2 (zs - ws) < 0.000001

instance KnownNat n => VectorSpace (StateVec n) where
  zero = UnsafeMkStateVec $ internalLen (sing :: Sing n) |> repeat 0
  z .: (UnsafeMkStateVec a) = UnsafeMkStateVec $ LA.scalar z * a
  (UnsafeMkStateVec a) +: (UnsafeMkStateVec b) = UnsafeMkStateVec $ a + b
  neg (UnsafeMkStateVec a) = UnsafeMkStateVec $ -a

instance KnownNat n => HilbertSpace (StateVec n) where
  (UnsafeMkStateVec zs) <.> (UnsafeMkStateVec ws) = zs LA.<.> ws

instance KnownNat n => Show (StateVec n) where
  show (UnsafeMkStateVec zs) = foldl show' "" $ zip [0..] (LA.toList zs)
    where show' str (i,z) = str ++ show z ++ "\t" ++ basis i ++ "\n"
          basis i = let bit j = if i `quot` 2^(n-j-1) `mod` 2 == 0
                                then '0' else '1'
                    in  "|" ++ fmap bit (take n [0..]) ++ ">"
          n = fromIntegral $ fromSing (sing :: Sing n)

internalLen :: (KnownNat n, Num a) => Sing n -> a
internalLen = (2 ^) . fromIntegral . fromSing 

-- | StateVec's are intialized to |0>
mkStateVec :: forall n . KnownNat n => StateVec n
mkStateVec = UnsafeMkStateVec $ internalLen (sing :: Sing n) |> (1 : repeat 0) 

-- | Make a statevector with an explicity specified size.
-- StateVec's are intialized to |0>
mkStateVec' :: Sing n -> StateVec n
mkStateVec' sn = UnsafeMkStateVec $ l |> (1 : repeat 0) 
  where l = fromIntegral $ fromSing sn

-- | A qubit is just a StateVec 1, initalized to |0>
mkQubit :: StateVec 1
mkQubit = UnsafeMkStateVec $ LA.fromList [1, 0]

normalize :: StateVec n -> StateVec n
normalize (UnsafeMkStateVec zs) = UnsafeMkStateVec $ LA.normalize zs

adjoint :: KnownNat n => StateVec n -> (StateVec n -> C)
adjoint qr = (qr <.>)

-- | The tensor product on elements of our Hilbert space
tensor :: StateVec n -> StateVec m -> StateVec (n + m)
tensor (UnsafeMkStateVec zs) (UnsafeMkStateVec ws) =
  UnsafeMkStateVec $ LA.flatten (zs `LA.outer` ws)

-- | Collapse a StateVec to a state compatable with qubit <n> being measured <bit>.
-- Only occurs physically with measurement, so internal use only.
collapse 
  :: forall n . KnownNat n 
  => Finite n -> Bit -> StateVec n -> StateVec n
collapse i b (UnsafeMkStateVec zs) = normalize . UnsafeMkStateVec $ zs * mask
  where
    mask   = l |> altseq
    altseq = replicate m ifZero ++ replicate m ifOne ++ altseq
    ifZero = if b == Zero then 1 else 0
    ifOne  = if b == One  then 1 else 0
    l      = internalLen (sing :: Sing n)
    m      = l `quot` (2 ^ (getFinite i + 1))

-- | Preforms a measurement of an induvidual qubit (indexed Finite n) 
-- in a quantum register.
measureQubit 
  :: (MonadRandom m, KnownNat n) 
  => Finite n -> StateT (StateVec n) m Bit
measureQubit i = do
  qr <- get
  r  <- getRandomR (0, 1)
  let qrZero = collapse i Zero qr
      qrOne  = collapse i One qr
      pOne   = realPart $ qrOne <.> qr -- guaranteed to be real, so this
  if r < pOne                          -- is just a type-cast.
    then put qrOne  >> pure One
    else put qrZero >> pure Zero

-- | Preform a measurement on all qubits in a quantum register, returning a 
-- computational basis state.
measure
  :: forall m n . (MonadRandom m, KnownNat n) 
  => StateT (StateVec n) m (CReg n)
measure = mkCReg <$> traverse measureQubit (take n [0 ..])
  where n = fromIntegral $ fromSing (sing :: Sing n)
