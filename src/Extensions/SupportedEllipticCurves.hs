module Extensions.SupportedEllipticCurves where

import Data.Bimap (fromList)
import Data.Binary (Get)
import Data.Function (on)
import Data.Ord (comparing)
import Utility.Binary (getWithLengthTag)
import Utility.BinaryEnum
import Utility.EllipticCurve
import Utility.EllipticCurve.Montgomery
import Utility.EllipticCurve.Weierstrass
import Utility.UInt (UInt)

data NamedCurve where
	NamedCurve :: EllipticCurve c => String -> c Integer n -> NamedCurve

name :: NamedCurve -> String
name (NamedCurve n _) = n

instance BinaryEnum NamedCurve (UInt 16) where
	equivalences = fromList
		[ (NamedCurve "secp256k1" secp256k1, 22)
		, (NamedCurve "secp384r1" secp384r1, 24)
		, (NamedCurve "x25519" x25519, 0x001d)
		]
instance Eq NamedCurve where
	(==) = (==) `on` name
instance Ord NamedCurve where
	compare = comparing name
instance Show NamedCurve where
	show = show . name

supportedEllipticCurvesIdentifier :: UInt 16
supportedEllipticCurvesIdentifier = 0x000A

getSupportedEllipticCurves :: Get [NamedCurve]
getSupportedEllipticCurves = getWithLengthTag @(UInt 16)