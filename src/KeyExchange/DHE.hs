module KeyExchange.DHE where

import Control.Monad.State.Lazy
import Data.Kind (Type)
import GHC.TypeNats (Nat)
import KeyExchange
-- import Numeric.Natural (Natural)
-- import System.Random
import Utility.ModularArithmetic
-- import Utility.UInt

import Random

newtype DHE (n :: Type) (p :: Nat) = DHE (Mod n p)

instance (Random g n, ValidMod n p) => KeyExchange (DHE n p) g where
	type PublicKey (DHE n p) = Mod n p
	type PrivateKey (DHE n p) = n
	genKeyPair (DHE g) = do
		private <- random
		return (g ^ private, private)