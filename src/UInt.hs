{-# LANGUAGE PatternSynonyms #-}

module UInt where

import Control.Monad (replicateM)
import Data.Binary
import Data.Bits
import Data.Foldable (foldl')
import Data.List.Split (chunksOf)
import Data.Proxy (Proxy (..))
import Data.Sized hiding (reverse, replicate, map, length, (++))
import GHC.TypeNats (KnownNat, Nat, natVal)

newtype UInt (n :: Nat) = UInt (Sized [] n Bool) deriving (Eq, Ord)

boolListToNum :: (Foldable t, Num a) => t Bool -> a
boolListToNum = foldl' (\x b -> x*2+(if b then 1 else 0)) 0

toWord8Chunk :: KnownNat n => UInt n -> [Word8]
toWord8Chunk (UInt n) = map boolListToNum $ chunksOf 8 $ padding ++ bits
	where
		bits = toList n
		padding = replicate ((8 - length bits) `mod` 8) False

fromWord8Chunk :: KnownNat n => [Word8] -> UInt n
fromWord8Chunk = foldl (\x y -> x*256 + fromIntegral y) 0

asInteger :: Integral a => (Integer -> Integer -> Integer) -> a -> a -> a
asInteger f a b = fromInteger $ f (toInteger a) (toInteger b)

instance KnownNat n => Binary (UInt n) where
	get = fromWord8Chunk <$> replicateM numChunks get
		where numChunks = ceiling $ fromIntegral (natVal (Proxy :: Proxy n)) / (8 :: Rational)
	put = mapM_ put . toWord8Chunk
instance KnownNat n => Enum (UInt n) where
	toEnum = fromIntegral
	fromEnum = fromIntegral
instance KnownNat n => Integral (UInt n) where
	quotRem = undefined
	toInteger (UInt n) = boolListToNum $ toList n
instance KnownNat n => Num (UInt n) where
	(+) = asInteger (+)
	(*) = asInteger (*)
	abs = id
	signum = const 1
	fromInteger = UInt . unsafeFromList' . reverse . flip map bitRange . testBit
		where bitRange = [0..fromIntegral (natVal (Proxy :: Proxy n)) - 1]
	(-) = asInteger (-)
instance KnownNat n => Real (UInt n) where
	toRational = toRational . toInteger
instance KnownNat n => Show (UInt n) where
	show = show . toInteger