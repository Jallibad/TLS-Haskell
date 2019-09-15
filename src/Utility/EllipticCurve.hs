module Utility.EllipticCurve
	( EllipticCurve (..)
	, encrypt
	) where

import Data.Kind (Type)
import GHC.TypeNats (Nat)
import Utility.ModularArithmetic (ValidMod)

-- |Class representing <https://en.wikipedia.org/wiki/Elliptic_curve Elliptic Curves>
class EllipticCurve (c :: Type -> Nat -> Type) where
	data Point c (n :: Type) (p :: Nat)

	-- |Add two points on an elliptic curve
	infixl 6 ~+
	(~+) :: ValidMod n p => Point c n p -> Point c n p -> Point c n p

	negate :: ValidMod n p => Point c n p -> Point c n p

	infixl 6 ~-
	(~-) :: ValidMod n p => Point c n p -> Point c n p -> Point c n p
	p1 ~- p2 = p1 ~+ Utility.EllipticCurve.negate p2

	infixr 7 ~*
	(~*) :: ValidMod n p => n -> Point c n p -> Point c n p

encrypt :: (EllipticCurve c, ValidMod n p) => Point c n p -> n -> n -> Point c n p -> (Point c n p, Point c n p)
encrypt g k nb pm = (k ~* g, pm ~+ (k*nb) ~* g)