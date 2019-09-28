module Utility.ModularArithmeticSpec where

import Data.Functor ((<&>))
import Data.Proxy (Proxy (..))
import GHC.TypeNats (SomeNat (..), someNatVal)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.SmallCheck as SmallCheck (property)
import Test.QuickCheck as QuickCheck (choose, property)
import Test.SmallCheck (forAll)
import Test.SmallCheck.Series (Serial (series), generate)
import Utility.ModularArithmetic

instance (Monad f, ValidMod a m) => Serial f (Mod a m) where
	series = generate $ flip take [0.. maxBound]

spec :: Spec
spec = do
	-- describe "fromInteger" $
	-- 	it "should be the same as the integer % m" $
	-- 		property $ forAll $ \(b :: Mod Integer 23) -> 
	describe "maxBound" $
		it "should equal the modulus - 1" $ 
			QuickCheck.property $ choose (1 :: Integer, 10^(10 :: Int)) <&> \n ->
				case someNatVal $ fromIntegral n of
					SomeNat (_ :: Proxy n) -> fromIntegral (maxBound :: Mod Integer n) == n - 1
	describe "modInverse" $
		it "an element times its inverse (if it has one) equals 1" $
			SmallCheck.property $ forAll $ \(b :: Mod Integer 23) -> maybe True (==1) $ (b*) <$> modInverse b