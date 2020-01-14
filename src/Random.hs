{-# LANGUAGE UndecidableInstances #-}

module Random where

import Control.Monad.State.Lazy (State, evalState, get, put, runState)
import Data.Constraint.Trivial
import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeLits
import qualified System.Random

class RandomGenerator (g :: Type) where
	type CanGenerate g a :: Constraint
class RandomGenerator g => Generatable g a where
	random :: CanGenerate g a => State g a
type Random g a = (CanGenerate g a, Generatable g a)

newtype SimpleTestGenerator a = HardcodedValues [a]
hardcoded :: [a] -> SimpleTestGenerator a
hardcoded = HardcodedValues . cycle
instance RandomGenerator (SimpleTestGenerator a) where
	type CanGenerate (SimpleTestGenerator a) b = a ~ b
instance Generatable (SimpleTestGenerator a) a where
	random :: State (SimpleTestGenerator a) a
	random = do
		HardcodedValues (x:xs) <- get
		put $ HardcodedValues xs
		return x

class NotInList x xs
type RepeatedHardcodedValues x =
	'Text "Trying to hardcode values of type '" ':<>:
	'ShowType x ':<>:
	'Text "' multiple times, this is most likely an error"
instance {-# OVERLAPPING #-} (TypeError (RepeatedHardcodedValues x)) => NotInList x (x, xs)
instance NotInList x Void
instance {-# OVERLAPPABLE #-} NotInList x xs => NotInList x (y, xs)

data CompoundGenerator a where
	Base :: CompoundGenerator Void
	Compound :: NotInList a b => SimpleTestGenerator a -> CompoundGenerator b -> CompoundGenerator (a, b)

instance RandomGenerator (CompoundGenerator Void) where
	type CanGenerate (CompoundGenerator Void) a = Unconstrained0
instance RandomGenerator (CompoundGenerator (a, b)) where
	type CanGenerate (CompoundGenerator (a, b)) c = CanGenerate (CompoundGenerator b) c

type NoHardcodedValues a =
	'Text "Cannot generate a value of type '" ':<>:
	'ShowType a ':<>:
	'Text "' because the given generator doesn't have hardcoded values for that type"
instance (TypeError (NoHardcodedValues a), Impossible0) => Generatable (CompoundGenerator Void) a where
	random = nope
instance {-# OVERLAPPABLE #-} Generatable (CompoundGenerator b) c => Generatable (CompoundGenerator (a, b)) c where
	random :: CanGenerate (CompoundGenerator b) c => State (CompoundGenerator (a, b)) c
	random = do
		Compound g gs <- get
		let (x, gs') = runState random gs
		put $ Compound g gs'
		return x
instance {-# OVERLAPPING #-} Generatable (CompoundGenerator (a, b)) a where
	random :: CanGenerate (CompoundGenerator (a, b)) a => State (CompoundGenerator (a, b)) a
	random = do
		Compound g gs <- get
		let (x, g') = runState random g
		put $ Compound g' gs
		return x

data RealGenerator where
	RealGenerator :: System.Random.RandomGen g => g -> RealGenerator
instance RandomGenerator RealGenerator where
	type CanGenerate RealGenerator a = System.Random.Random a
instance System.Random.Random a => Generatable RealGenerator a where
	random = do
		RealGenerator g <- get
		let (result, g') = System.Random.random g
		put $ RealGenerator g'
		return result

getThreeRandoms :: (RandomGenerator g, Random g a) => g -> [a]
getThreeRandoms g = flip evalState g $ do
	a <- random
	b <- random
	c <- random
	return [a, b, c]

class CanCompose (list :: Type) (a :: Type) where
	type ReturnType list a :: Type
	type ArgType list a :: Type
	compose :: ArgType list a -> [a] -> ReturnType list a

instance CanCompose Void a where
	type ReturnType Void a = CompoundGenerator (a, Void)
	type ArgType Void a = CompoundGenerator Void
	compose :: CompoundGenerator Void -> [a] -> CompoundGenerator (a, Void)
	compose base xs = Compound (HardcodedValues xs) base
instance CanCompose Void b => CanCompose (b, Void) a where
	type ReturnType (b, Void) a = [b] -> CompoundGenerator (a, (b, Void))
	type ArgType (b, Void) a = [b] -> ReturnType Void b
	compose :: ([b] -> ReturnType Void b) -> [a] -> ReturnType (b, Void) a
	-- compose = undefined
	compose f as bs = Compound (HardcodedValues as) $ f bs

-- instance (current ~ ReturnType c b, CanCompose (CompoundGenerator c) c b) => CanCompose ([b] -> current) (a, (b, c)) a where
-- 	type ReturnType ([b] -> current) (a, (b, c)) a = [b] -> CompoundGenerator (a, (b, c))
-- 	compose :: ([b] -> current) -> [a] -> [b] -> CompoundGenerator (a, (b, c))
-- 	compose f as bs = Compound (HardcodedValues as) $ f bs

-- class Compose v a where
-- 	hardcode' :: [a] -> v
-- 	prepend :: [b] -> v ->
-- instance Compose (CompoundGenerator (a, Void)) a where
-- 	hardcode' :: [a] -> CompoundGenerator (a, Void)
-- 	hardcode' xs = Compound (HardcodedValues $ cycle xs) Base
-- instance forall a b v. Compose v b => Compose ([b] -> v) a where
-- 	hardcode' :: [a] -> [b] -> v
-- 	hardcode' = undefined
	-- hardcode' xs = \ys -> hardcode' 
		-- where
		-- 	thing :: v
		-- 	thing = hardcode' ys

-- class Compose v a b where
-- 	hardcode' :: CompoundGenerator b -> SimpleTestGenerator a -> v
-- instance Compose (CompoundGenerator (a, Void)) a Void where
-- 	hardcode' :: CompoundGenerator Void -> SimpleTestGenerator a -> CompoundGenerator (a, Void)
-- 	hardcode' base next = Compound next base
-- instance Compose v c (a, b) => Compose (CompoundGenerator (a, b) -> v) a b where
-- 	hardcode' :: CompoundGenerator b -> SimpleTestGenerator a -> (CompoundGenerator (a, b) -> v)
-- 	hardcode' base next = hardcode' $Compound next base