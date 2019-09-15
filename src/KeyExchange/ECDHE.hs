module KeyExchange.ECDHE where

import Control.Concurrent.MVar
-- import Data.Kind (Type)
import System.Random
-- import Utility.EllipticCurve.Weierstrass
import Utility.UInt

class KeyExchange a where
	type PublicKey a
	type PrivateKey a
	server :: RandomGen g => MVar g -> IO (PublicKey a, PrivateKey a)
	client :: RandomGen g => MVar g -> IO (PublicKey a, PrivateKey a)

genPrivateKey :: RandomGen g => g -> UInt 32
genPrivateKey = undefined

-- data ECDHE (k :: Type)
-- instance KeyExchange (ECDHE k) where
-- 	type PublicKey (ECDHE k) = k
-- 	type PrivateKey (ECDHE k) = k
	-- server g = 

-- server :: RandomGen g => g -> EllipticCurve n p -> Point n p -> 