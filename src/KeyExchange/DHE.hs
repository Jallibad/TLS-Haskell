module KeyExchange.DHE where

import Control.Monad.State.Lazy
import Data.Kind (Type)
import GHC.TypeNats (Nat)
import KeyExchange
-- import Numeric.Natural (Natural)
import System.Random
import Utility.ModularArithmetic
-- import Utility.UInt

newtype DHE (n :: Type) (p :: Nat) = DHE (Mod n p)

instance (Random n, ValidMod n p) => KeyExchange (DHE n p) where
	type PublicKey (DHE n p) = Mod n p
	type PrivateKey (DHE n p) = n
	genKeyPair (DHE g) = do
		(private, gen) <- random <$> get
		put gen
		return (g ^ private, private)