module Utility.ModularArithmetic
	( Mod
	, ValidMod
	, modInverse
	, quadraticResidue
	) where

import Control.Arrow ((***), first)
import Data.Function (on)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat, natVal, KnownNat)
import System.Random

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
modInverse 0 = Nothing
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

instance ValidMod a m => Random (Mod a m) where
	randomR (low, high) = first fromIntegral . randomR (toInteger low, toInteger high)
	random = randomR (minBound, maxBound)

legendreSymbol :: forall a m. ValidMod a m => Mod a m -> Mod a m
legendreSymbol a = a ^ ((natVal (Proxy :: Proxy m) - 1) `div` 2)

isQuadraticNonResidue :: ValidMod a m => Mod a m -> Bool
isQuadraticNonResidue = (== -1) . legendreSymbol

tonelliShanks :: forall a m. ValidMod a m => Mod a m -> Mod a m
tonelliShanks n = looper s (z^q) (n^q) (n ^ ((q+1) `div` 2))
	where
		p = natVal (Proxy :: Proxy m)
		(q, s :: Integer) = until (odd . fst) ((`div` 2) *** succ) (p - 1, 0)
		z = fromMaybe (error "Can't find any quadratic nonresidues") $ find @[] isQuadraticNonResidue [0..]
		looper _ _ 0 _ = 0
		looper _ _ 1 r = r
		looper m c t r = looper i (b^(2 :: Integer)) (t * b ^ (2 :: Integer)) (r*b)
			where
				i = until (\i' -> t ^ (2 :: Integer) ^ i' == 1) succ 0
				b = c ^ (2 :: Integer) ^ (m - i - 1)

quadraticResidue :: ValidMod a m => Mod a m -> [Mod a m]
quadraticResidue n = case legendreSymbol n of
	-1 -> []
	1 -> let r = tonelliShanks n in [-r, r]
	_ -> undefined