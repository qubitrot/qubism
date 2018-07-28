{-|
Module      : Qubism.QReg
Description : Types and functions for quantum registers
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Qubism.QReg
  ( QReg (UnsafeMkQReg)
  , mkQReg
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

newtype QReg (n :: Nat) =
  UnsafeMkQReg (LA.Vector C)

-- | To provide "close enough" equality testing.
instance Eq (QReg n) where
  UnsafeMkQReg zs == UnsafeMkQReg ws = 
     LA.norm_2 (zs - ws) < 0.000001

instance KnownNat n => VectorSpace (QReg n) where
  zero = UnsafeMkQReg $ internalLen (sing :: Sing n) |> repeat 0
  z                .: (UnsafeMkQReg a) = UnsafeMkQReg $ LA.scalar z * a
  (UnsafeMkQReg a) +: (UnsafeMkQReg b) = UnsafeMkQReg $ a + b
  neg (UnsafeMkQReg a) = UnsafeMkQReg $ -a

instance KnownNat n => HilbertSpace (QReg n) where
  (UnsafeMkQReg zs) <.> (UnsafeMkQReg ws) = zs LA.<.> ws

instance Show (QReg n) where
  show (UnsafeMkQReg zs) = foldl show' "" $ LA.toList zs
    where show' str z = str ++ show z ++ "\n"

internalLen :: (KnownNat n, Num a) => Sing n -> a
internalLen = (2 ^) . fromIntegral . fromSing 

-- | QReg's are intialized to |0>
mkQReg :: forall n . KnownNat n => QReg n
mkQReg = UnsafeMkQReg $ internalLen (sing :: Sing n) |> (1 : repeat 0) 

-- | A qubit is just a QReg 1, initalized to |0>
mkQubit :: QReg 1
mkQubit = UnsafeMkQReg $ LA.fromList [1, 0]

normalize :: QReg n -> QReg n
normalize (UnsafeMkQReg zs) = UnsafeMkQReg $ LA.normalize zs

-- | The tensor product on elements of our Hilbert space
tensor :: QReg n -> QReg m -> QReg (n + m)
tensor (UnsafeMkQReg zs) (UnsafeMkQReg ws) =
  UnsafeMkQReg $ LA.flatten (zs `LA.outer` ws)

-- | Collapse a QReg to a state compatable with qubit <n> being measured <bit>.
-- Only occurs physically with measurement, so internal use only.
collapse :: forall n . KnownNat n => Finite n -> Bit -> QReg n -> QReg n
collapse i b (UnsafeMkQReg zs) = normalize . UnsafeMkQReg $ zs * mask
  where
    mask   = l |> altseq
    altseq = replicate m ifZero ++ replicate m ifOne ++ altseq
    ifZero = if b == Zero then 1 else 0
    ifOne  = if b == One  then 1 else 0
    l      = internalLen (sing :: Sing n)
    m      = l `quot` (2 ^ (getFinite i + 1))

-- | Preforms a measurement of an induvidual qubit (indexed Finite n) 
-- in a quantum register.
measureQubit :: (MonadRandom m, KnownNat n) => Finite n -> StateT (QReg n) m Bit
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
  :: forall m n . (MonadRandom m, KnownNat n) => StateT (QReg n) m (CReg n)
measure = mkCReg <$> traverse measureQubit (take n [0 ..])
  where n = fromIntegral $ fromSing (sing :: Sing n)