{-# LANGUAGE UndecidableInstances #-}

module Utility.UInt
	( UInt
	) where

import Control.Arrow (first, (***))
import Control.Monad (replicateM)
import Data.Binary (Binary, Word8, get, put)
import Data.Bits (Bits (..), FiniteBits (..), testBitDefault)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List.Split (chunksOf)
import Data.Proxy (Proxy (..))
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Sized hiding (fmap, reverse, replicate, map, length, (++))
import Data.Word
import GHC.TypeNats (KnownNat, Nat, natVal)
import System.Random

data UIntFast (n :: Nat) where
	UInt16 :: Word16 -> UIntFast 16
	UIntBase :: UInt n -> UIntFast n

safeConstruct :: Sing n -> UInt n -> UIntFast n
safeConstruct sng num = case sng of
	SNat -> undefined
	_ -> UIntBase num

-- newtype UIntFast n = UIntFast (UIntFinder n)

-- instance KnownNat n => Show (UIntFast n) where
-- 	show uint = show n

-- type family UIntFinder (n :: Nat) where
-- 	UIntFinder 16	= Word16
-- 	UIntFinder 32	= Word32
-- 	UIntFinder n	= UInt n

-- type family IsBaseType (n :: Nat) where
-- 	IsBaseType 16	= 'False
-- 	IsBaseType 32	= 'False
-- 	IsBaseType n	= 'True

-- class (KnownNat numBits, Num (UInt' numBits)) =>
-- 		UnsignedInt (numBits :: Nat) where
-- 	type UInt' numBits

-- instance forall n. (KnownNat n, Num (UIntFinder n)) => UnsignedInt n where
-- 	type UInt' n = UIntFinder n --Decomp (IsBaseType n) n

-- class Num (Decomp isBaseType numBits) =>
-- 		Helper (isBaseType :: Bool) (numBits :: Nat) where
-- 	type Decomp isBaseType numBits

-- instance KnownNat n => Helper 'True n where
-- 	type Decomp 'True n = UInt n

-- instance Num (UIntFinder n) => Helper 'False n where
-- 	type Decomp 'False n = UIntFinder n

-- test :: UnsignedInt n => UInt' n -> UInt' n
-- test n = n + 1

-- testInstance :: UInt' 31
-- testInstance = (test :: UInt' 31 -> UInt' 31) (0 :: UInt' 31)

type BitList (n :: Nat) = Sized [] n Bool
newtype UInt (n :: Nat) = UInt (BitList n) deriving (Eq, Ord)

boolListToNum :: (Foldable t, Num a) => t Bool -> a
boolListToNum = foldl' (\x b -> x*2+(if b then 1 else 0)) 0

toWord8Chunk :: KnownNat n => UInt n -> [Word8]
toWord8Chunk (UInt n) = map boolListToNum $ chunksOf 8 $ padding ++ bits
	where
		bits = toList n
		padding = replicate ((8 - length bits) `mod` 8) False

fromWord8Chunk :: KnownNat n => [Word8] -> UInt n
fromWord8Chunk = foldl (\x y -> x*256 + fromIntegral y) 0

asVector :: KnownNat n => (BitList n -> BitList n) -> UInt n -> UInt n
asVector f (UInt v) = UInt $ f v

asInteger :: Integral a => (Integer -> Integer -> Integer) -> a -> a -> a
asInteger f a b = fromInteger $ f (toInteger a) (toInteger b)

instance KnownNat n => Binary (UInt n) where
	get = fromWord8Chunk <$> replicateM numChunks get
		where numChunks = ceiling $ fromIntegral (natVal (Proxy :: Proxy n)) / (8 :: Rational)
	put = mapM_ put . toWord8Chunk

instance KnownNat n => Bits (UInt n) where
	(.&.) = asInteger (.&.)
	(.|.) = asInteger (.|.)
	xor = asInteger xor
	complement = asVector $ fmap not
	shift n i = fromInteger $ shift (fromIntegral n) i
	rotate n i = boolListToNum $ map (\x -> testBit n $ (x - i) `mod` len) $ reverse [0.. len - 1]
		where len = finiteBitSize n
	bitSizeMaybe = Just . finiteBitSize
	bitSize = finiteBitSize
	isSigned = const False
	testBit = testBitDefault
	bit = (2^)
	popCount = undefined

instance KnownNat n => Enum (UInt n) where
	toEnum = fromIntegral
	fromEnum = fromIntegral

instance KnownNat n => FiniteBits (UInt n) where
	finiteBitSize = const $ fromIntegral $ natVal (Proxy :: Proxy n)

instance KnownNat n => Integral (UInt n) where
	quotRem a b = (fromIntegral *** fromIntegral) $ quotRem (fromIntegral a :: Integer) (fromIntegral b)
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

instance KnownNat n => Bounded (UInt n) where
	minBound = 0
	maxBound = negate 1

instance KnownNat n => Random (UInt n) where
	randomR (low, high) = first fromIntegral . randomR (toInteger low, toInteger high)
	random = randomR (minBound, maxBound)