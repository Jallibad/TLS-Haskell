{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utility.BinaryEnum
	( BinaryEnum
	, createEquivalences
	, equivalences
	) where

import Data.Bimap (Bimap, fromList, lookup, lookupR)
import Data.Binary (Binary, get, put)
import Data.Maybe (fromMaybe)

-- | A convenience typeclass to shorten instantiations of the Binary typeclass for enumeration types.
-- Gaps in the equivalence range are valid e.g. @fromList [(A, 1), (B, 3), (C, 10)]@
class (Ord a, Ord n, Eq n, Binary n) => BinaryEnum a n | a -> n where
	equivalences :: Bimap a n

-- | The workhorse of the BinaryEnum typeclass, uses the @equivalences@ bimap to look up the corresponding Binary enum value
instance {-# OVERLAPPABLE #-} BinaryEnum a n => Binary a where
	get = do
		x <- flip lookupR equivalences <$> get
		case x of
			Nothing -> fail "Invalid enum value"
			Just y -> pure y
	put = put . fromMaybe (error "Missing enum in BinaryEnum definition") . flip Data.Bimap.lookup equivalences

createEquivalences :: (Ord a, Enum a, Ord b) => [b] -> Bimap a b
createEquivalences = fromList . (zip $ enumFrom $ toEnum 0)