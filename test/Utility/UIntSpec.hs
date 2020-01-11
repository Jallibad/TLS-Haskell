module Utility.UIntSpec where

import Data.Functor ((<&>))
import Data.Proxy (Proxy)
import GHC.TypeNats (SomeNat (..), someNatVal)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck as QuickCheck --(choose, property)
import Utility.UInt

thing :: Testable b => (Integer, Integer) -> (SomeNat -> b) -> Gen b
thing range f = f . someNatVal . fromInteger <$> choose range

spec :: Spec
spec = do
	describe "UInt" $ do
		it "round trips positive numbers" $ do
			QuickCheck.property $ thing (1, 256) $ \(SomeNat (_ :: Proxy n)) ->
				choose (0, maxBound) <&> \x -> fromInteger (toInteger (x :: UInt n)) == x