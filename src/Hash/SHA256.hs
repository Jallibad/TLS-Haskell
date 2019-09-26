module Hash.SHA256 where

import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Numeric (showHex)
import Utility.UInt

type HashValues = (UInt 32, UInt 32, UInt 32, UInt 32, UInt 32, UInt 32, UInt 32, UInt 32)

-- | SHA256 round constants.
roundConstants :: Vector (UInt 32)
roundConstants =
	[ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
	, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
	, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
	, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
	, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
	, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
	, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
	, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
	]

initialValues :: HashValues
initialValues =
	( 0x6a09e667
	, 0xbb67ae85
	, 0x3c6ef372
	, 0xa54ff53a
	, 0x510e527f
	, 0x9b05688c
	, 0x1f83d9ab
	, 0x5be0cd19
	)

buildW :: (Bits a, Num a) => Vector a -> Int -> Vector a
buildW = (fst .) . curry (until ((>=64) . snd) $ uncurry update2)

update1 :: Vector (UInt 32) -> HashValues -> Int -> (HashValues, Int)
update1 w (a, b, c, d, e, f, g, h) i = ((temp1 + temp2, a, b, c, d + temp1, e, f, g), i + 1)
	where
		s1 = (e `rotateR` 6) `xor` (e `rotateR` 11) `xor` (e `rotateR` 25)
		ch = (e .&. f) `xor` (complement e .&. g)
		temp1 = h + s1 + ch + (roundConstants ! i) + (w ! i)
		s0 = (a `rotateR` 2) `xor` (a `rotateR` 13) `xor` (a `rotateR` 22)
		maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
		temp2 = s0 + maj

update2 :: (Bits a, Num a) => Vector a -> Int -> (Vector a, Int)
update2 v i = (v <> V.singleton ((v ! (i - 16)) + s0 + (v ! (i - 7)) + s1), i + 1)
	where
		s0 = (v_15 `rotateR` 7) `xor` (v_15 `rotateR` 18) `xor` (v_15 `shiftR` 3)
		s1 = (v_2 `rotateR` 17) `xor` (v_2 `rotateR` 19) `xor` (v_2 `shiftR` 10)
		v_15 = v ! (i - 15)
		v_2 = v ! (i - 2)

mergeHashValues :: (Num a, Bits a) => HashValues -> a
mergeHashValues (h0, h1, h2, h3, h4, h5, h6, h7) =
	shiftL (fromIntegral h0) 224
	.|. shiftL (fromIntegral h1) 192
	.|. shiftL (fromIntegral h2) 160
	.|. shiftL (fromIntegral h3) 128
	.|. shiftL (fromIntegral h4) 96
	.|. shiftL (fromIntegral h5) 64
	.|. shiftL (fromIntegral h6) 32
	.|. fromIntegral h7

-- | Combine four bytes, assuming they are given in big-endian order.
mergeWordsBE :: UInt 8 -> UInt 8 -> UInt 8 -> UInt 8 -> UInt 32
mergeWordsBE a b c d =
	shiftL (fromIntegral a) 24
	.|. shiftL (fromIntegral b) 16
	.|. shiftL (fromIntegral c) 8
	.|. fromIntegral d

-- | Unpack binary data into a list of 32-bit words, assuming big-endian encoding.
unpackWord32BE :: ByteString -> [UInt 32]
unpackWord32BE = go' . map fromIntegral . BS.unpack
	where
		go' (a:b:c:d:xs) = mergeWordsBE a b c d : go' xs
		go' _ = []

-- | Split binary data at the given interval.
splitEvery :: Int -> ByteString -> [ByteString]
splitEvery n bs
	| BS.length bs <= fromIntegral n = [bs]
	| otherwise = case BS.splitAt (fromIntegral n) bs of (x, rest) -> x:splitEvery n rest

rep :: Integral a => Integer -> [a]
rep 0 = []
rep n = fromIntegral r : rep q
	where (q, r) = quotRem n 256

-- | Convert an integer to padded binary data, assuming big-endian encoding.
integerToByteStringBE :: Int -> Integer -> ByteString
integerToByteStringBE padding = BS.pack . until ((>=padding) . length) (0:) . reverse . rep

-- | Pad an input for SHA-1 or SHA256.
padMessage :: ByteString -> [Vector (UInt 32)]
padMessage bs = fmap (V.fromList . unpackWord32BE) $ splitEvery 64 $ bs <> padding <> len
	where
		paddingDiff = 56 - mod (BS.length bs) 64
		paddingLength = fromIntegral $ if paddingDiff <= 0 then 512 - paddingDiff else paddingDiff
		paddingWords = case replicate paddingLength 0 of
			(x:xs) -> (x .|. 0b10000000) : xs
			[] -> []
		padding = BS.pack paddingWords
		len = integerToByteStringBE 8
			. flip mod (2^(64 :: Int))
			. (*8) . fromIntegral $ BS.length bs

-- padMessage' :: Binary m => m -> ByteString
-- padMessage' (decode -> bs) = 
-- 	where
-- 		len = BS.length bs * 8

hashChunk :: HashValues -> Vector (UInt 32) -> HashValues
hashChunk hs@(h0, h1, h2, h3, h4, h5, h6, h7) msg = (h0 + a, h1 + b, h2 + c, h3 + d, h4 + e, h5 + f, h6 + g, h7 + h)
	where
		(a, b, c, d, e, f, g, h) = fst $ until ((>=64) . snd) (uncurry $ update1 (buildW msg 16)) (hs, 0)

showByteString :: ByteString -> String
showByteString = Prelude.concatMap (`showHex` "") . BS.unpack

-- | Compute the SHA256 hash of the given bytes.
sha256 :: ByteString -> ByteString
sha256 = integerToByteStringBE 32 . mergeHashValues . foldl hashChunk initialValues . padMessage