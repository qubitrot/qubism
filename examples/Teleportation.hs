{-|
Description : Example for quantum teleportation
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE DataKinds #-}

import Control.Monad.Random
import Control.Monad.Trans.State
import Qubism

-- | Quantum teleportation of one bit
teleport1
  :: MonadRandom m
  => StateVec 1     -- ^ Alice's qubit
  -> m (StateVec 3) -- ^ Resulting total state. Alice's state is teleported 
                -- to qubit 3 (indexed 2).
teleport1 a = do
  let pair  = cnot 0 1 <> onJust 0 hadamard #> mkStateVec :: StateVec 2 
  let total = a `tensor` pair              
  flip execStateT total $ do
    gate $ cnot 0 1
    gate $ onJust 0 hadamard
    c0 <- measureQubit 0
    c1 <- measureQubit 1
    gate $ ifBit c0 $ onJust 2 pauliZ
    gate $ ifBit c1 $ onJust 2 pauliX
  
main :: IO ()
main = do
  let qr = unitary 0.3 0.2 0.1 #> mkQubit
  print qr
  total <- teleport1 qr
  print total
