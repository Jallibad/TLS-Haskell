module KeyExchange where

import Control.Monad.State.Lazy (State)
-- import System.Random (RandomGen)
import Random

class KeyExchange a g where
	type PublicKey a
	type PrivateKey a
	genKeyPair :: RandomGenerator g => a -> State g (PublicKey a, PrivateKey a)