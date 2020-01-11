{-# LANGUAGE UndecidableInstances #-}

module Utility.NatProofs where

import Control.Arrow
import Data.Singletons.Prelude.Num ((%+), (%-))
import Data.Singletons.Prelude.Enum (Succ)
import Data.Singletons.Decide
import Data.Singletons.TypeLits
import Data.Kind
import Data.Type.Equality
-- import Data.Type.Nat
import GHC.TypeLits.Compare
import GHC.TypeNats
import Data.Singletons
import Data.Type.Predicate
import Data.Type.Predicate.Logic
import Data.Type.Predicate.Param
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

data CanSubtract' :: Nat -> Predicate Nat
type instance Apply (CanSubtract' a) b = ((b - a) + a) :~: b
-- data (8 <=? (n * 8)) ~ 'True
-- instance forall n. Provable (CanSubtract' n ==> ) where

data Fun (a :: Nat -> Type) (b :: Nat -> Type) (n :: Nat) where
	Case :: KnownNat n => {
		unwrap :: (a n -> b n)
	} -> Fun a b n
data Induction a b where
	Step :: {
		unstep :: forall n. KnownNat n => Fun a b n -> Fun a b (Succ n)
	} -> Induction a b

data LessThanEqualTo :: ParamPred Nat Nat
type instance Apply (LessThanEqualTo a) b = (a <=? b) :~: 'True
data LessThan :: ParamPred Nat Nat
type instance Apply (LessThan a) b = Apply (LessThanEqualTo a &&& (Not (EqualTo a))) b
-- instance (KnownNat a', Succ a ~ a') => Provable ((LessThan a) ==> (LessThanEqualTo a')) where
-- 	prove :: forall b. Sing b -> (((a <=? b) :~: 'True), Refuted (a :~: b)) -> (((Succ a) <=? b) :~: 'True)
-- 	prove b (proofOfOrdering, proofOfInequality) = case (Sing :: Sing a') %<=? b of
-- 		LE proof -> proof
-- 		_ -> undefined

induction :: forall a b (base :: Nat) (n :: Nat). (KnownNat base, KnownNat n, base <= n) => Fun a b base -> Induction a b -> Fun a b n
induction baseCase step = case (sing :: Sing base) %~ (sing :: Sing n) of
	Proved Refl -> baseCase
	-- Disproved _ -> induction ((unstep step :: Fun a b base -> Fun a b (Succ base)) baseCase :: Fun a b (Succ base)) step
induction' :: (KnownNat base, KnownNat n, KnownNat 1, base <= n) => (a base -> b base) -> (forall m. KnownNat m => (a m -> b m) -> a (Succ m) -> b (Succ m)) -> a n -> b n
induction' f g = unwrap (induction (Case f) (Step $ Case <<< unwrap ^>> g))