{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utility.BinaryEnum
	( BinaryEnum
	, createEquivalences
	, equivalences
	) where

import Control.Monad.Catch (MonadThrow)
import Data.Bimap (Bimap, fromList, lookup, lookupR)
import Data.Binary (Binary, get, put)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

-- | A convenience typeclass to shorten instantiations of the Binary typeclass for enumeration types.
-- Gaps in the equivalence range are valid e.g. @fromList [(A, 1), (B, 3), (C, 10)]@
class (Ord a, Ord n, Eq n, Binary n) => BinaryEnum a n | a -> n where
	equivalences :: Bimap a n
	enumToNum :: MonadThrow m => n -> m a
	enumToNum = flip lookupR equivalences
	numToEnum :: a -> n
	numToEnum = fromMaybe (error "Missing enum in BinaryEnum definition") . flip lookup equivalences

-- | The workhorse of the BinaryEnum typeclass, uses the @equivalences@ bimap to look up the corresponding Binary enum value
instance {-# OVERLAPPABLE #-} BinaryEnum a n => Binary a where
	get = get >>= maybe (fail "Invalid enum value") pure . enumToNum
	put = put . numToEnum

createEquivalences :: (Ord a, Enum a, Ord b) => [b] -> Bimap a b
createEquivalences = fromList . zip (enumFrom $ toEnum 0)