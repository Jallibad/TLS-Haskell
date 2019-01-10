module KeyExchange.ECDHE where

import Data.Conduit
import Control.Concurrent.MVar
import Utility.EllipticCurve.Weierstrass
import Utility.UInt
import System.Random

class KeyExchange a where
	type family PublicKey a
	type family PrivateKey a
	server :: RandomGen g => MVar g -> IO (PublicKey a, PrivateKey a)
	client :: RandomGen g => MVar g -> IO (PublicKey a, PrivateKey a)

genPrivateKey :: RandomGen g => g -> UInt 32
genPrivateKey = undefined

data ECDHE k
instance KeyExchange (ECDHE k) where
	PublicKey = k
	PrivateKey = k
	-- server g = 

-- server :: RandomGen g => g -> EllipticCurve n p -> Point n p -> 