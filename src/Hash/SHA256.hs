module Hash.SHA256
	( SHA256
	) where

import Data.Binary-- (encode)
import Data.Binary.Get (runGet)
import Data.Bits (Bits (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS --(length, pack, unpack)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.List (uncons)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V (fromList)
import Data.Word (Word32)
import Hash
import Utility.Binary --(getAll)
import Utility.Bytes (Bytes, fromUInt)
import Utility.UInt (UInt)

data SHA256

instance HashFunction SHA256 64 32 where
	hash = (mergeHashValues @Word32) . foldl' hashChunk initialValues . padMessage

type HashValues a = (a, a, a, a, a, a, a, a)

-- | SHA256 round constants.
roundConstants :: Num a => Vector a
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

initialValues :: Num a => HashValues a
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

mergeHashValues :: Integral a => HashValues a -> Bytes 32
mergeHashValues (h0, h1, h2, h3, h4, h5, h6, h7) = fromUInt $
	shiftL (fromIntegral h0) 224
	.|. shiftL (fromIntegral h1) 192
	.|. shiftL (fromIntegral h2) 160
	.|. shiftL (fromIntegral h3) 128
	.|. shiftL (fromIntegral h4) 96
	.|. shiftL (fromIntegral h5) 64
	.|. shiftL (fromIntegral h6) 32
	.|. fromIntegral h7

padMessage :: Binary a => ByteString -> [Vector a]
padMessage = fmap V.fromList . runGet (getAll' $ getNBytes 64) . preprocess

preprocess :: ByteString -> ByteString
preprocess = mconcat . (<$> [id, padding, len]) . (&)
	where
		padding = padZeros . getPaddingLength
		len = encode . (* (8 :: UInt 64)) . fromIntegral . BS.length

padZeros :: Int -> ByteString
padZeros = maybe "" (\(x, xs) -> BS.pack $ (x .|. 0b10000000) : xs) . uncons . flip replicate 0

getPaddingLength :: Num b => ByteString -> b
getPaddingLength bs = fromIntegral $ if paddingDiff <= 0 then 512 - paddingDiff else paddingDiff
	where paddingDiff = 56 - mod (BS.length bs) 64

hashChunk :: (Bits a, Num a) => HashValues a -> Vector a -> HashValues a
hashChunk hs@(h0, h1, h2, h3, h4, h5, h6, h7) msg = (h0 + a, h1 + b, h2 + c, h3 + d, h4 + e, h5 + f, h6 + g, h7 + h)
	where
		(a, b, c, d, e, f, g, h) = fst $ until ((>=64) . snd) (thing' msg) (hs, 0)

thing' :: (Bits a, Num a) => Vector a -> (HashValues a, Int) -> (HashValues a, Int)
thing' = uncurry . update1 . buildW

buildW :: (Bits a, Num a) => Vector a -> Vector a
buildW = fst . until ((>=64) . snd) update2 . (,16)

update2 :: (Bits a, Num a) => (Vector a, Int) -> (Vector a, Int)
update2 (v, i) = (v <> [((v ! (i - 16)) + s0 + (v ! (i - 7)) + s1)], i + 1)
	where
		s0 = (v_15 `rotateR` 7) `xor` (v_15 `rotateR` 18) `xor` (v_15 `shiftR` 3)
		s1 = (v_2 `rotateR` 17) `xor` (v_2 `rotateR` 19) `xor` (v_2 `shiftR` 10)
		v_15 = v ! (i - 15)
		v_2 = v ! (i - 2)

update1 :: (Bits a, Num a) => Vector a -> HashValues a -> Int -> (HashValues a, Int)
update1 w (a, b, c, d, e, f, g, h) i = ((temp1 + temp2, a, b, c, d + temp1, e, f, g), i + 1)
	where
		s1 = (e `rotateR` 6) `xor` (e `rotateR` 11) `xor` (e `rotateR` 25)
		ch = (e .&. f) `xor` (complement e .&. g)
		temp1 = h + s1 + ch + (roundConstants ! i) + (w ! i)
		s0 = (a `rotateR` 2) `xor` (a `rotateR` 13) `xor` (a `rotateR` 22)
		maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
		temp2 = s0 + maj
