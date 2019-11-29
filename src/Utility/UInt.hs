{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utility.UInt
	( UInt
	) where

import Control.Arrow (first, (***))
import Control.Monad (replicateM)
import Data.Binary (Binary, Word8, get, put)
import Data.Bits (Bits (..), FiniteBits (..), testBitDefault)
import Data.Foldable (foldl')
-- import qualified Data.Kind
import Data.List.Split (chunksOf)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq, (|>))
-- import Data.Singletons
import Data.Sized hiding (fmap, reverse, replicate, map, length, (++), (|>))
import Data.Word
import GHC.TypeNats (KnownNat, Nat, natVal)
import Language.Haskell.TH
import System.Random
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
	rotate n i = boolListToNum $ map (\x -> testBit n $ (x - i) `mod` len) $ reverse [0.. len - 1]
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
	fromInteger = UInt . unsafeFromList' . reverse . flip map bitRange . testBit
		where bitRange = [0..fromIntegral (natVal (Proxy :: Proxy n)) - 1]
	(-) = asInteger (-)

instance KnownNat n => Real (UIntBase n) where
	toRational = toRational . toInteger

instance KnownNat n => Show (UIntBase n) where
	show = show . toInteger

instance KnownNat n => Bounded (UIntBase n) where
	minBound = 0
	maxBound = negate 1

instance KnownNat n => Random (UIntBase n) where
	randomR (low, high) = first fromIntegral . randomR (toInteger low, toInteger high)
	random = randomR (minBound, maxBound)

data UInt (n :: Nat) where
	UInt16 :: Word16 -> UInt 16
	UInt32 :: Word32 -> UInt 32
	UIntBase :: UIntBase n -> UInt n

-- fromIntegerExplicit :: KnownNat n => Integer -> UInt n
-- fromIntegerExplicit n =
-- 	chooseIfEqual @Nat @16 (UInt16 $ fromInteger n) $
-- 	chooseIfEqual @Nat @32 (UInt32 $ fromInteger n) $
-- 	UIntBase $ fromInteger n

-- fromIntegerExplicit :: forall n. KnownNat n => Integer -> UInt n
-- fromIntegerExplicit = unsuspend $
-- 	chooseIfEqual @Nat @16 (Suspend $ UInt16 . fromInteger) $
-- 	-- chooseIfEqual (Suspend $ UInt32 . fromInteger) $
-- 	(Suspend $ UIntBase . fromInteger :: Suspension Integer UInt n)

deriveInstance ''UInt ''Show
deriveInstance ''UInt ''Eq
deriveInstance ''UInt ''Ord
deriveInstanceWith ''UInt ''Num $ sequence
	[ valD (varP 'fromInteger) (normalB $ createConstructor ''UInt [|fromInteger|]) []
	]
-- deriveInstanceWith ''UInt ''Integral
-- 	[d|$('quotRem) a b = (fromIntegral *** fromIntegral) $ quotRem (fromIntegral a :: Integer) (fromIntegral b)|]

-- instance KnownNat n => Enum (UInt n) where
-- 	toEnum = fromIntegral
-- 	fromEnum = fromIntegral

toWord8Chunk :: Integral a => a -> Seq Word8
toWord8Chunk 0 = []
toWord8Chunk n = toWord8Chunk (n `div` 256) |> fromIntegral (mod n 256)

fromWord8Chunk :: (Foldable t, Integral a, Num b) => t a -> b
fromWord8Chunk = foldl (\x y -> x*256 + fromIntegral y) 0

-- instance KnownNat n => Binary (UInt n) where
-- 	get = fromWord8Chunk <$> replicateM numChunks get
-- 		where numChunks = ceiling $ fromIntegral (natVal (Proxy :: Proxy n)) / (8 :: Rational)
-- 	put = mapM_ put . toWord8Chunk