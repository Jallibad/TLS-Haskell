module Utility.EllipticCurve.Weierstrass where

-- import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits hiding (Mod)
import Utility.EllipticCurve
import Utility.ModularArithmetic
import Utility.UInt (UInt)

data Weierstrass n p = Weierstrass {_a :: Mod n p, _b :: Mod n p} deriving (Eq)
instance EllipticCurve Weierstrass where
	data Point Weierstrass n p = Point {curve :: Weierstrass n p, x :: Mod n p, y :: Mod n p}

	p@(Point curvep xp yp) ~+ q@(Point _curveq xq _yq) = Point curvep xr yr
		where
			m = slope p q
			xr = m^(2 :: Int)-xp-xq
			yr = m*(xp-xr)-yp
	negate (Point curve x y) = Point curve x $ Prelude.negate y

	0 ~* (Point curve _ _) = Point curve 0 0
	1 ~* p = p
	2 ~* p = p ~+ p
	n ~* p
		| odd n = p ~+ (n - 1) ~* p
		| otherwise = (n `div` 2) ~* 2 ~* p

instance (Show n, ValidMod n p) => Show (Weierstrass n p) where
	show (Weierstrass a b) = mconcat ["y^2 = x^3", showTerm a, "x", showTerm b]
		where showTerm t = (if t >= 0 then "+" else "-") ++ show (fromIntegral t :: n)

-- instance Ord n => Ord (Point Weierstrass n p) where
-- 	compare = comparing (\(_, x, y) -> (x,y))

instance Show n => Show (Point Weierstrass n p) where
	show (Point _ x y) = show (x, y)

slope :: ValidMod n p => Point Weierstrass n p -> Point Weierstrass n p -> Mod n p
slope (Point curve1 x1 y1) (Point curve2 x2 y2)
	| curve1 /= curve2 = error "Points on different curves"
	| x1 == x2 = (3*x1^(2 :: Int) + _a curve1) `div` (2*y1)
	| otherwise = (y1-y2) `div` (x1-x2)

evalCurve :: ValidMod n p => Weierstrass n p -> Mod n p -> Set (Mod n p)
evalCurve (Weierstrass a b) x0 = Set.fromList $ quadraticResidue $ x0^(3 :: Int)+a*x0+b

-- pointsOnCurve :: ValidMod n p => Weierstrass n p -> Set (Point Weierstrass n p)
-- pointsOnCurve c = Set.unions $ map (\x -> Set.map (Point c x) $ evalCurve c x) [0..maxBound]

-- infixl 6 ~+
-- (~+) :: ValidMod n p => Point n p -> Point n p -> Point n p
-- p@(Point curvep xp yp) ~+ q@(Point _curveq xq _yq) = Point curvep xr yr
-- 	where
-- 		m = slope p q
-- 		xr = m^(2 :: Int)-xp-xq
-- 		yr = m*(xp-xr)-yp

encrypt :: ValidMod n p => Point Weierstrass n p -> n -> n -> Point Weierstrass n p -> (Point Weierstrass n p, Point Weierstrass n p)
encrypt g k nb pm = (k ~* g, pm ~+ (k*nb) ~* g)

nistP256 :: Weierstrass Integer (2^256 - 2^224 + 2^192 + 2^96 - 1)
nistP256 = Weierstrass (-3) 41058363725152142129326129780047268409114441015993725554835256314039467401291

secp256k1 :: Point Weierstrass (UInt 256) (2^256 - 2^32 - 977)
secp256k1 = Point curve 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
	where curve = Weierstrass 0 7

secp384r1 :: Weierstrass Integer (2^384 - 2^128 - 2^96 + 2^32 - 1)
secp384r1 = Weierstrass (-3) 27580193559959705877849011840389048093056905856361568521428707301988689241309860865136260764883745107765439761230575