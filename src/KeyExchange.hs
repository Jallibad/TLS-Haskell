module KeyExchange where

import Control.Monad.State.Lazy (State)
import System.Random (RandomGen)

class KeyExchange a where
	type PublicKey a
	type PrivateKey a
	genKeyPair :: RandomGen g => a -> State g (PublicKey a, PrivateKey a)