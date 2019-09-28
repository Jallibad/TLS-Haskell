module KeyExchange.ECDHE where

import Control.Monad.State.Lazy
import Data.Kind (Type)
import GHC.TypeNats (Nat)
import KeyExchange
import System.Random
import Utility.EllipticCurve
-- import Utility.EllipticCurve.Weierstrass
import Utility.ModularArithmetic (ValidMod)
-- import Utility.UInt

newtype ECDHE (c :: Type -> Nat -> Type) (n :: Type) (p :: Nat) = ECDHE (Point c n p)
instance (EllipticCurve c, Random n, ValidMod n p) => KeyExchange (ECDHE c n p) where
	type PublicKey (ECDHE c n p) = Point c n p
	type PrivateKey (ECDHE c n p) = n
	genKeyPair (ECDHE g) = do
		(private, gen) <- random <$> get
		put gen
		return (private ~* g, private)