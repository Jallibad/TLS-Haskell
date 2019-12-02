module Utility.NatProofs where

import Data.Singletons.Prelude.Enum (Succ)
import Data.Singletons.Decide
-- import Data.Type.Equality
-- import Data.Type.Nat
import GHC.TypeNats
import Data.Type.Predicate
import Data.Type.Predicate.Logic

newtype Thing1 n = Thing1 (n :~: 0)
newtype Thing2 n = Thing2 (Succ n :~: 1)
instance Provable ((TyPred Thing1) ==> (TyPred Thing2)) where
	prove :: Prove ((TyPred Thing1) ==> (TyPred Thing2))
	prove = undefined

-- type BaseCase f = f 'Z
-- type BaseCase' f = f (FromGHC 0)
-- type InductionStep f = forall (n :: Data.Type.Nat.Nat). SNatI n => f n -> f ('S n)
-- type InductionStep' f = forall (n :: GHC.TypeNats.Nat) (m :: Data.Type.Nat.Nat).
-- 							(KnownNat n, (FromGHC n) ~ m, ((ToGHC ('S m)) ~ (Succ n))) => f m -> f ('S m)

-- test :: InductionStep' f
-- test = undefined

-- inductionStep' :: forall f (n :: GHC.TypeNats.Nat) (m :: Data.Type.Nat.Nat).
-- 	(KnownNat n, FromGHC n ~ m, (1 + ToGHC m) ~ (n + 1)) =>
-- 	(f m -> f ('S m)) -> (forall (m :: Data.Type.Nat.Nat). SNatI m => f m -> f ('S m))
-- inductionStep' r = undefined

-- type Thing f n = forall m. (KnownNat n, (FromGHC n) ~ m, SNatI m) => BaseCase' f -> InductionStep' f -> f m
-- -- test :: (KnownNat n, SNatI (FromGHC n), FromGHC n ) => BaseCase' f -> InductionStep' f -> f (FromGHC n)
-- test :: forall f n m. (KnownNat n, (FromGHC n) ~ m, SNatI m) => f 'Z -> InductionStep' f -> f m
-- -- test baseCase inductionStep = induction baseCase (inductionStep :: InductionStep' f)
-- test baseCase inductionStep = case eqNat @(FromGHC n) @m of
-- 	Just Refl -> induction baseCase (inductionStep' inductionStep)