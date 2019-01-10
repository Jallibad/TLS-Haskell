module Utility.ModularArithmetic
	( Mod
	, ValidMod
	, modInverse
	, quadraticResidue
	) where

import Data.Function (on)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat, natVal, KnownNat)

newtype Mod a (m :: Nat) = Mod a deriving (Eq, Ord)
type ValidMod a m = (Integral a, KnownNat m)

unMod :: Mod a m -> a
unMod (Mod a) = a

instance Show a => Show (Mod a m) where
	show = show . unMod

instance ValidMod a m => Enum (Mod a m) where
	fromEnum = fromIntegral . unMod
	toEnum = fromIntegral

instance ValidMod a m => Num (Mod a m) where
	(+) = (fromIntegral .) . ((+) `on` unMod)
	(*) = (fromIntegral .) . ((*) `on` unMod)
	negate = fromIntegral . negate . unMod
	abs = id
	signum = id
	fromInteger = Mod . fromInteger . flip mod (natVal $ Proxy @m)

instance (Ord a, ValidMod a m) => Real (Mod a m) where
	toRational = toRational . toInteger . unMod

instance ValidMod a m => Integral (Mod a m) where
	quotRem a (modInverse -> Just b)	= (a*b, 0)
	quotRem a _							= (0, a)
	toInteger = toInteger . unMod

diffs :: (a -> a -> b) -> [a] -> [b]
diffs _ [] = []
diffs _ (_:[]) = []
diffs f (x:y:xs) = (f x y) : diffs f (y:xs)

modInverse :: forall a m. ValidMod a m => Mod a m -> Maybe (Mod a m)
modInverse a
	| last r == 1 = Just $ fromIntegral $ last $ init s
	| otherwise = Nothing
	where
		b = fromInteger $ natVal $ Proxy @m :: a
		r = takeWhile (/=0) (unMod a : b : zipWith (-) r (zipWith (*) q (tail r)))
		s = 1 : 0 : zipWith (-) s (zipWith (*) q (tail s))
		q = diffs div r

instance ValidMod a m => Bounded (Mod a m) where
	minBound = 0
	maxBound = 0 - 1

quadraticResidue :: ValidMod a m => Mod a m -> [Mod a m]
quadraticResidue n = filter ((==n) . (^(2 :: Int))) [0..maxBound]