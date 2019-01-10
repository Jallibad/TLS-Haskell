module Utility.EllipticCurve.Montgomery where

import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits hiding (Mod)
import Utility.ModularArithmetic

data EllipticCurve n p = EllipticCurve {_a :: Mod n p, _b :: Mod n p} deriving (Eq)

instance (Show n, ValidMod n p) => Show (EllipticCurve n p) where
	show (EllipticCurve a b) = "y^2 = x^3" ++ (showTerm a) ++ "x" ++ (showTerm b)
		where showTerm t = (if t >= 0 then "+" else "-") ++ show (fromIntegral t :: n)

data Point n p = Point {_curve :: EllipticCurve n p, _x :: Mod n p, _y :: Mod n p} deriving (Eq)

instance Ord n => Ord (Point n p) where
	compare = comparing (\(Point _ x y) -> (x,y))

instance Show n => Show (Point n p) where
	show (Point _ x y) = show (x,y)

slope :: ValidMod n p => Point n p -> Point n p -> Mod n p
slope (Point curve1 x1 y1) (Point curve2 x2 y2)
	| curve1 /= curve2 = error "Points on different curves"
	| x1 == x2 = (3*x1^(2 :: Int)+(_a curve1)) `div` (2*y1)
	| otherwise = (y1-y2) `div` (x1-x2)

evalCurve :: ValidMod n p => EllipticCurve n p -> Mod n p -> Set (Mod n p)
evalCurve (EllipticCurve a b) x0 = Set.fromList $ quadraticResidue $ x0^(3 :: Int)+a*x0+b

pointsOnCurve :: ValidMod n p => EllipticCurve n p -> Set (Point n p)
pointsOnCurve c = Set.unions $ map (\x -> Set.map (Point c x) $ evalCurve c x) [0..maxBound]

infixl 6 ~+
(~+) :: ValidMod n p => Point n p -> Point n p -> Point n p
p@(Point curvep xp yp) ~+ q@(Point _curveq xq _yq) = Point curvep xr yr
	where
		m = slope p q
		xr = m^(2 :: Int)-xp-xq
		yr = m*(xp-xr)-yp

negate :: ValidMod n p => Point n p -> Point n p
negate (Point curve x y) = Point curve x $ Prelude.negate y

infixl 6 ~-
(~-) :: ValidMod n p => Point n p -> Point n p -> Point n p
p ~- q = p ~+ Utility.EllipticCurve.Weierstrass.negate q

infixr 7 ~*
(~*) :: ValidMod n p => n -> Point n p -> Point n p
0 ~* (Point curve _ _) = Point curve 0 0
1 ~* p = p
2 ~* p = p ~+ p
n ~* p
	| odd n = p ~+ (n - 1) ~* p
	| otherwise = (n `div` 2) ~* 2 ~* p

encrypt :: ValidMod n p => Point n p -> n -> n -> Point n p -> (Point n p, Point n p)
encrypt g k nb pm = (k ~* g, pm ~+ (k*nb) ~* g)

-- x25519 :: EllipticCurve Integer (2^255 - 19)
-- x25519 = EllipticCurve 486662 1