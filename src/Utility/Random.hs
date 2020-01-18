{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Utility.Random where

import Control.Monad.State.Lazy --(State, evalState, get, put, runState)
import Data.Constraint.Trivial
import Data.Dynamic
import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeLits
import qualified System.Random
import Type.Reflection

class RandomGenerator (g :: Type) where
	type CanGenerate g a :: Constraint
class (RandomGenerator g, CanGenerate g a) => Random g a where
	random :: MonadState g m => m a

newtype SimpleTestGenerator a = HardcodedValues [a]
instance Typeable a => Show (SimpleTestGenerator a) where
	show x = case typeOf x of
		App _ t -> show t
hardcoded :: [a] -> SimpleTestGenerator a
hardcoded = HardcodedValues . cycle
instance RandomGenerator (SimpleTestGenerator a) where
	type CanGenerate (SimpleTestGenerator a) b = a ~ b
instance Random (SimpleTestGenerator a) a where
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

class Show' list where
	showHelper :: CompoundGenerator list -> [String]
instance Show' Void where
	showHelper = const []
instance (Typeable a, Typeable b, Show' b) => Show' (a, b) where
	showHelper (Compound x xs) = show x : showHelper xs
instance Show' a => Show (CompoundGenerator a) where
	show x = mconcat ["Generator: ", show $ showHelper x]

instance RandomGenerator (CompoundGenerator Void) where
	type CanGenerate (CompoundGenerator Void) a = Unconstrained0
instance RandomGenerator (CompoundGenerator (a, b)) where
	type CanGenerate (CompoundGenerator (a, b)) c = CanGenerate (CompoundGenerator b) c

type NoHardcodedValues a =
	'Text "Cannot generate a value of type '" ':<>:
	'ShowType a ':<>:
	'Text "' because the given generator doesn't have hardcoded values for that type"
instance (TypeError (NoHardcodedValues a), Impossible0) => Random (CompoundGenerator Void) a where
	random = nope
instance {-# OVERLAPPABLE #-} Random (CompoundGenerator b) c => Random (CompoundGenerator (a, b)) c where
	random = do
		Compound g gs <- get
		let (x, gs') = runState random gs
		put $ Compound g gs'
		return x
instance {-# OVERLAPPING #-} CanGenerate (CompoundGenerator b) a => Random (CompoundGenerator (a, b)) a where
	random = do
		Compound g gs <- get
		let (x, g') = runState random g
		put $ Compound g' gs
		return x

data RealGenerator where
	RealGenerator :: System.Random.RandomGen g => g -> RealGenerator
instance RandomGenerator RealGenerator where
	type CanGenerate RealGenerator a = System.Random.Random a
instance System.Random.Random a => Random RealGenerator a where
	random = do
		RealGenerator g <- get
		let (result, g') = System.Random.random g
		put $ RealGenerator g'
		return result

-- getThreeRandoms :: (RandomGenerator g, Random g Int, Random g Char) => g -> [String]
-- getThreeRandoms g = flip evalState g $ do
-- 	(a :: Int)	<- random
-- 	(b :: Int)	<- random
-- 	(c :: Char)	<- random
-- 	(d :: Int)	<- random
-- 	return [show a, show b, show c, show d]

-- type family Compose (x :: Type) (xs :: Type) where
-- 	Compose x ([a] -> xs) = [a] -> Compose x xs
-- 	Compose x (CompoundGenerator xs) = CompoundGenerator (x, xs)

-- class Helper (xs :: Type) where
-- 	type Condition x xs :: Constraint
-- 	help :: Condition x xs => [x] -> Compose x xs
-- instance Helper (CompoundGenerator Void) where
-- 	type Condition a (CompoundGenerator Void) = Unconstrained0
-- 	help :: [x] -> CompoundGenerator (x, Void)
-- 	help x = Compound (hardcoded x) Base
-- instance (Helper (CompoundGenerator c), Condition b (CompoundGenerator c)) => Helper ([b] -> CompoundGenerator (b, c)) where
-- 	type Condition a ([b] -> CompoundGenerator (b, c)) = NotInList a (b, c)
-- 	help :: Condition a ([b] -> CompoundGenerator (b, c)) => [a] -> [b] -> CompoundGenerator (a, (b, c))
-- 	help as = Compound (hardcoded as) . help @(CompoundGenerator c)