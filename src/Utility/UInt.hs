{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utility.UInt
	( UInt
	, toWord8Chunk
	) where

import Control.Arrow (first, (***))
import Control.Monad (replicateM)
import Data.Binary (Binary, get, getWord8, put)
import Data.Bits (Bits (..), FiniteBits (..), testBitDefault)
import Data.Foldable (foldl')
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq, (|>))
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude.Enum (Succ)
import Data.Sized hiding (fmap, reverse, replicate, map, length, (++), (|>))
import Data.Type.Equality ((:~:)(Refl))
import Data.Word
import GHC.Exts (Constraint)
import GHC.TypeLits.Compare
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
	rotate n i = boolListToNum $ fmap (\x -> testBit n $ (x - i) `mod` len) $ reverse [0.. len - 1]
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
	fromInteger = UInt . unsafeFromList' . reverse . flip fmap bitRange . testBit
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

toBytes :: forall (n :: Nat). (KnownNat n, KnownNat (n * 8)) => UInt (n * 8) -> Bytes n
toBytes n = case sameNat (Proxy @n) (Proxy @0) of
	Just Refl -> []
	Nothing -> undefined

type p :=> q = forall a. p => (q => a) -> a
type Proof p = () :=> p
data Predicate (predicate :: k -> Constraint) = Predicate

type BaseCase		c = Proof (c 0)
type InductiveStep	c = forall n. KnownNat n => c n :=> c (Succ n)

class Natural (n :: Nat)

inductiveNat :: forall c n. KnownNat n => Predicate c -> BaseCase c -> InductiveStep c -> Proof (c n)
inductiveNat pc baseCase inductiveStep = case (sing @n) %~ (sing @0) of
	Proved Refl -> baseCase
	Disproved refutation -> undefined
	-- Nothing -> inductiveNat pc baseCase (inductiveStep :: c (n - 1) :=> c n) :: Proof (c (n - 1))

-- inductiveNat pc base step = case (Proxy @n) %<=? (Proxy @0) of
-- 	LE Refl -> base
-- 	NLE Refl Refl -> inductiveNat @c @(n - 1) pc base step

-- type Test n = ('GT ~ CmpNat n 0) :=> (((n - 1) + 1) ~ n)

-- class 'GT ~ CmpNat n 0 => AllGreaterThanZeroCanSubtract 

-- type AllGreaterThanZeroCanSubtract (CmpNat n 0 ~ Sing GT => (n - 1) + 1 ~ n)

-- class KnownNat n => Induction n p q where
-- 	induction :: p n :=> q n

-- instance Induction 0 (	Succ) (1 +)

class (KnownNat n, KnownNat (n * 8)) => FromBytes n where
	fromBytes :: Bytes n -> UInt (n * 8)

instance FromBytes 0 where
	fromBytes Empty = 0

instance (KnownNat n, KnownNat (n * 8), (Succ m) ~ n, FromBytes m) => FromBytes n where
	fromBytes (xs :|> x) = fromIntegral x + 256 * fromIntegral (fromBytes xs)

-- fromBytes :: forall (n :: Nat). (SingI (CmpNat n 0), KnownNat n, KnownNat (n * 8)) => Bytes n -> UInt (n * 8)
-- fromBytes = case (sing :: Sing (CmpNat n 0)) %~ (sing :: Sing 'GT) of
-- 	-- Proved Refl -> \((xs :: Bytes (n - 1)) :|> x) -> (fromIntegral $ fromBytes xs) * 256 + fromIntegral x
-- 	Disproved _ -> const 0

toWord8Chunk :: forall n. KnownNat n => UInt n -> Seq Word8
toWord8Chunk 0 = []
toWord8Chunk n = toWord8Chunk (n `div` 256) |> fromIntegral (mod n 256)

fromWord8Chunk :: (Foldable t, Integral a, Num b) => t a -> b
fromWord8Chunk = foldl' (\x y -> x*256 + fromIntegral y) 0

instance forall a b. (KnownNat a, KnownNat b, (a * 8) ~ b) => Binary (UInt b) where
	get = fromWord8Chunk <$> replicateM numChunks getWord8
		where numChunks = ceiling $ fromIntegral (natVal (Proxy :: Proxy b)) / (8 :: Rational)
	put = put . toBytes