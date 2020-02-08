{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utility.UInt
	( UInt
	, fromBytes
	, toBytes
	, toWord8Chunk
	) where

import Control.Arrow (first, (***))
import Control.Monad (replicateM)
import Data.Binary (Binary, get, getWord8, put)
import Data.Bits (Bits (..), FiniteBits (..), testBitDefault)
import Data.Foldable (foldl')
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq, reverse)
import Data.Sized (Sized, toList, unsafeFromList')
import Data.Word
import GHC.TypeNats --(type (*), type (-), KnownNat, Nat, natVal, sameNat)
import System.Random (Random (..))
import Utility.Bytes
import Utility.TH.CaseBasedData
import Utility.TH.DeriveInstancesByUnwrapping

type BitList (n :: Nat) = Sized [] n Bool
newtype UIntBase (n :: Nat) = UInt (BitList n) deriving (Eq, Ord)

boolListToNum :: (Foldable t, Num a) => t Bool -> a
boolListToNum = foldl' (\x b -> x*2+(if b then 1 else 0)) 0

asVector :: KnownNat n => (BitList n -> BitList n) -> UIntBase n -> UIntBase n
asVector f (UInt v) = UInt $ f v

asInteger :: Integral a => (Integer -> Integer -> Integer) -> a -> a -> a
asInteger f a b = fromInteger $ f (toInteger a) (toInteger b)

instance KnownNat n => Bits (UIntBase n) where
	(.&.) = asInteger (.&.)
	(.|.) = asInteger (.|.)
	xor = asInteger xor
	complement = asVector $ fmap not
	shift n i = fromInteger $ shift (fromIntegral n) i
	rotate n i = boolListToNum $ (\x -> testBit n $ (x - i) `mod` len) <$> Prelude.reverse [0.. len - 1]
		where len = finiteBitSize n
	bitSizeMaybe = Just . finiteBitSize
	bitSize = finiteBitSize
	isSigned = const False
	testBit = testBitDefault
	bit = (2^)
	popCount = undefined

instance KnownNat n => Enum (UIntBase n) where
	toEnum = fromIntegral
	fromEnum = fromIntegral

instance KnownNat n => FiniteBits (UIntBase n) where
	finiteBitSize = const $ fromIntegral $ natVal (Proxy :: Proxy n)

instance KnownNat n => Integral (UIntBase n) where
	quotRem a b = (fromIntegral *** fromIntegral) $ quotRem (fromIntegral a :: Integer) (fromIntegral b)
	toInteger (UInt n) = boolListToNum $ toList n

instance KnownNat n => Num (UIntBase n) where
	(+) = asInteger (+)
	(*) = asInteger (*)
	abs = id
	signum = const 1
	fromInteger = UInt . unsafeFromList' . Prelude.reverse . flip fmap bitRange . testBit
		where bitRange = [0..fromIntegral (natVal (Proxy :: Proxy n)) - 1]
	(-) = asInteger (-)

instance KnownNat n => Real (UIntBase n) where
	toRational = toRational . toInteger

instance KnownNat n => Show (UIntBase n) where
	show = show . toInteger

data UInt (n :: Nat) where
	UInt8		:: Word8		-> UInt 8
	UInt16		:: Word16		-> UInt 16
	UInt32		:: Word32		-> UInt 32
	UIntBase	:: UIntBase n	-> UInt n

deriveInstance ''UInt ''Show
deriveInstance ''UInt ''Eq
deriveInstance ''UInt ''Ord
deriveInstanceWith ''UInt ''Num
	[ ('fromInteger, createConstructor ''UInt [|fromInteger|])
	]
deriveInstanceWith ''UInt ''Integral
	[ ('quotRem, [|\a b -> (fromIntegral *** fromIntegral) $ quotRem (toInteger a) (fromIntegral b)|])
	]
deriveInstanceWith ''UInt ''Bits
	[ ('zeroBits, [|0|])
	, ('bit, [|fromInteger . bit|])
	]

instance KnownNat n => Enum (UInt n) where
	toEnum = fromIntegral
	fromEnum = fromIntegral

instance KnownNat n => Real (UInt n) where
	toRational = toRational . toInteger

instance KnownNat n => Bounded (UInt n) where
	minBound = 0
	maxBound = negate 1

instance KnownNat n => Random (UInt n) where
	randomR (low, high) = first fromIntegral . randomR (toInteger low, toInteger high)
	random = randomR (minBound, maxBound)

-- newtype Thing n = Thing {unthing :: UInt (n * 8)}
toBytes :: forall (n :: Nat). (KnownNat 1, KnownNat n, KnownNat (n * 8)) => UInt (n * 8) -> Bytes n
toBytes = unsafeFromList . toWord8Chunk
-- toBytes = induction' (const []) f . Thing
-- 	where
-- 		f :: KnownNat m => (Thing m -> Bytes m) -> Thing (Succ m) -> Bytes (Succ m)
-- 		f g (Thing n) = undefined
-- toBytes = Thing ^>> induction (const []) ((undefined :: (1 <=? (n * 8)) ~ 'True => UInt (n * 8) -> Bytes n) . unthing)

fromBytes :: forall (n :: Nat). (KnownNat n, KnownNat (n * 8)) => Bytes n -> UInt (n * 8)
fromBytes = undefined
-- fromBytes = unthing . induction' (const $ Thing 0) (\g (xs :|> x) -> Thing $ (fromIntegral $ unthing $ g xs) * 256 + fromIntegral x)
		
-- fromBytes = unthing . induction (const $ Thing 0 :: Bytes 0 -> Thing 0) undefined
-- fromBytes = case prove @CanInduct n of
-- 	Left (decrement :: Sing (n - 1))-> case prove @(HasAPredecessor ==> CanSubtract) n decrement of
-- 		Refl -> \((xs :: Bytes (n - 1)) :|> x) -> (fromIntegral $ fromBytes xs) * 256 + fromIntegral x
-- 	Right Refl -> const 0
-- 	where n = sing :: Sing n
-- fromBytes = case (sing :: Sing (CmpNat n 0)) %~ (sing :: Sing 'GT) of
-- 	Proved Refl -> \((xs :: Bytes (n - 1)) :|> x) -> (fromIntegral $ fromBytes xs) * 256 + fromIntegral x
-- 	Disproved _ -> const 0

toWord8Chunk :: forall a b. (KnownNat a, KnownNat b, (a * 8) ~ b) => UInt b -> Seq Word8
-- toWord8Chunk 0 = []
-- toWord8Chunk n = toWord8Chunk (fromInteger $ toInteger n `div` 256 :: UInt n) |> fromIntegral (toInteger n `mod` 256)
toWord8Chunk n = (\s -> fromIntegral $ shiftR n (fromIntegral s*8) .&. 255) <$> Data.Sequence.reverse [0.. size - 1]
	where size = natVal (Proxy :: Proxy a)

fromWord8Chunk :: (Foldable t, Integral a, Num b) => t a -> b
fromWord8Chunk = foldl' (\x y -> x*256 + fromIntegral y) 0

instance forall a b. (KnownNat a, KnownNat b, (a * 8) ~ b) => Binary (UInt b) where
	get = fromWord8Chunk <$> replicateM numChunks getWord8
		where numChunks = ceiling $ fromIntegral (natVal (Proxy :: Proxy b)) / (8 :: Rational)
	put = put . toBytes