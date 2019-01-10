{-# LANGUAGE TypeFamilyDependencies #-}

module Utility.EllipticCurve where

import Data.Kind
import GHC.TypeNats
import Utility.ModularArithmetic
-- import qualified Utility.EllipticCurve.Weierstrass as Sub

class EllipticCurve (a :: Type -> Nat -> Type) where
	type family Point a (n :: Type) (p :: Nat)
	infixl 6 ~+
	(~+) :: forall n p. ValidMod n p => Point a n p -> Point a n p-> Point a n p
	negate :: forall n p. ValidMod n p => Point a n p -> Point a n p
	infixl 6 ~-
	(~-) :: forall n p. ValidMod n p => Point a n p -> Point a n p -> Point a n p
	infixr 7 ~*
	(~*) :: forall n p. ValidMod n p => n -> Point a n p -> Point a n p
	-- a ~- b = (Utility.EllipticCurve.negate :: Point a n p -> Point a n p) b