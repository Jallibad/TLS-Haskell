module Utility.EllipticCurve.Montgomery
	( x25519
	) where

import Data.List (uncons)
-- import Data.Ord (comparing)
-- import Data.Set (Set)
-- import qualified Data.Set as Set
import GHC.TypeLits hiding (Mod)
import Utility.EllipticCurve
import Utility.ModularArithmetic

data Montgomery n p = Montgomery {_a :: Mod n p, _b :: Mod n p} deriving (Eq)

instance EllipticCurve Montgomery where
	data Point Montgomery n p = Point {_curve :: Montgomery n p, _x :: Mod n p} deriving (Eq)

	p@(Point curvep xp) ~+ q@(Point _curveq xq) = Point curvep xr
		where
			m = slope p q
			xr = m^(2 :: Int)-xp-xq

	negate = undefined

	0 ~* (Point curve _) = Point curve 0
	1 ~* p = p
	2 ~* p = p ~+ p
	n ~* p
		| odd n = p ~+ (n - 1) ~* p
		| otherwise = (n `div` 2) ~* 2 ~* p

instance (Show n, ValidMod n p) => Show (Montgomery n p) where
	show (Montgomery a b) = mconcat ["y^2 = x^3", showTerm a, "x^2", showTerm b, "x"]
		where showTerm t = (if t >= 0 then "+" else "-") ++ show (fromIntegral t :: n)

instance Show n => Show (Point Montgomery n p) where
	show (Point _ x) = show x

slope :: ValidMod n p => Point Montgomery n p -> Point Montgomery n p -> Mod n p
slope (Point curve1 x1) (Point curve2 x2)
	| curve1 /= curve2 = error "Points on different curves"
	| x1 == x2 = (3*x1^(2 :: Int) + _a curve1) `div` (2*y1)
	| otherwise = (y1-y2) `div` (x1-x2)
	where
		y1 = findY curve1 x1
		y2 = findY curve2 x2

-- evalCurve :: ValidMod n p => Montgomery n p -> Mod n p -> Set (Mod n p)
-- evalCurve (Montgomery a b) x0 = Set.fromList $ quadraticResidue $ x0^(3 :: Int)+a*x0^(2 :: Int)+b*x0

findY :: ValidMod n p => Montgomery n p -> Mod n p -> Mod n p
findY (Montgomery a b) x0 = maybe (error "No y for this x") fst $ uncons
	$ quadraticResidue $ x0^(3 :: Int)+a*x0^(2 :: Int)+b*x0

x25519 :: Integral n => Montgomery n (2^255 - 19)
x25519 = Montgomery 486662 1

-- x25519 :: Integral n => Point Montgomery n (2^255 - 19)
-- x25519 = Point curve25519 9