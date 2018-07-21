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
  ( QReg
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

import           Control.Monad.Random hiding (fromList)
import           Control.Monad.Trans.State
import           Data.Complex
import           Numeric.LinearAlgebra (C,(|>),(<.>))
import qualified Numeric.LinearAlgebra as LA

import Qubism.CReg

newtype QReg (n :: Nat) =
  QReg (LA.Vector C)

instance Show (QReg n) where
  show (QReg zs) = foldl show' "" $ LA.toList zs
    where show' str z = str ++ show z ++ "\n"

-- | QReg's are intialized to |0>
mkQReg :: Sing n -> QReg n
mkQReg s = QReg $ l |> (1 : repeat 0) where l = (2 ^) $ fromSing s

-- | A qubit is just a QReg 1, initalized to |0>
mkQubit :: QReg 1
mkQubit = QReg $ LA.fromList [1, 0]

normalize :: QReg n -> QReg n
normalize (QReg zs) = QReg $ LA.normalize zs

-- | The tensor product on elements of our Hilbert space
tensor :: QReg n -> QReg m -> QReg (n + m)
tensor (QReg zs) (QReg ws) = QReg $ LA.flatten (zs `LA.outer` ws)

-- | The sesquilinear inner product on our Hilbert space
inner :: QReg n -> QReg m -> C
inner (QReg zs) (QReg ws) = LA.conj zs <.> ws

-- | Collapse a QReg to a state compatable with qubit <n> being measured <bit>.
-- Only occurs physically with measurement, so internal use only.
collapse :: forall n . KnownNat n => Finite n -> Bit -> QReg n -> QReg n
collapse i b (QReg zs) = normalize . QReg $ zs * mask
  where
    mask   = l |> altseq
    altseq = replicate m ifZero ++ replicate m ifOne ++ altseq
    ifZero = if b == Zero then 1 else 0
    ifOne  = if b == One then 1 else 0
    l      = (2 ^) $ fromSing (sing :: Sing n)
    m      = l `quot` (2 ^ (getFinite i + 1))

-- | Preforms a measurement of an induvidual qubit (indexed Finite n) 
-- in a quantum register.
measureQubit :: (MonadRandom m, KnownNat n) => Finite n -> StateT (QReg n) m Bit
measureQubit i = do
  qr <- get
  r  <- getRandomR (0, 1)
  let qrZero = collapse i Zero qr
      qrOne  = collapse i One qr
      pOne   = realPart $ qrOne `inner` qr -- guaranteed to be real, so this
  case r < pOne of                         -- is just a type-cast.
    True  -> put qrOne >> pure One
    False -> put qrZero >> pure Zero

-- | Preform a measurement on all qubits in a quantum register, returning a 
-- computational basis state.
measure
  :: forall m n . (MonadRandom m, KnownNat n) => StateT (QReg n) m (CReg n)
measure = traverse measureQubit (take n [0 ..]) >>= pure . mkCReg
  where n = fromIntegral $ fromSing (sing :: Sing n)
