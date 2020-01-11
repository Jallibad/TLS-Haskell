module Hash.SHA256Spec where

import Hash (HashFunction (hash))
import Hash.SHA256 (SHA256)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
	describe "SHA256" $ do
		it "hashes the same as the test vectors" $ do
			hash @SHA256 "" `shouldBe`		[0xe3b0c442, 0x98fc1c14, 0x9afbf4c8, 0x996fb924, 0x27ae41e4, 0x649b934c, 0xa495991b, 0x7852b855]
			hash @SHA256 "abc" `shouldBe`	[0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223, 0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad]