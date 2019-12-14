{-# LANGUAGE UndecidableInstances #-}

module Utility.NatProofs where

import Data.Singletons.Prelude.Num ((%+), (%-))
import Data.Singletons.Prelude.Enum (Succ)
import Data.Singletons.Decide
import Data.Singletons.TypeLits
import Data.Kind
import Data.Type.Equality
-- import Data.Type.Nat
import GHC.TypeNats
import Data.Singletons
import Data.Type.Predicate
import Data.Type.Predicate.Logic
import Data.Void (absurd)

data HasASuccessor :: Predicate Nat
type instance Apply HasASuccessor n = Sing (Succ n)
instance Provable HasASuccessor where
	prove n = n %+ (sing :: Sing 1)

data HasAPredecessor :: Predicate Nat
type instance Apply HasAPredecessor n = Sing (n - 1)
type CanInduct = (HasAPredecessor ||| EqualTo 0)
instance Provable CanInduct where
	prove :: Prove CanInduct
	prove n = case (sing :: Sing 0) %~ n of
		Proved Refl -> Right Refl
		Disproved _ -> Left $ n %- (sing :: Sing 1)

data CanSubtract :: Predicate Nat
type instance Apply CanSubtract n = ((n - 1) + 1) :~: n
instance Provable (HasAPredecessor ==> CanSubtract) where
	prove :: forall n. Sing n -> Sing (n - 1) -> ((n - 1) + 1) :~: n
	prove n m = case (m %+ (sing :: Sing 1)) %~ n of
		Proved Refl -> Refl
		Disproved refuted -> undefined