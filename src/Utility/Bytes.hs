module Utility.Bytes where

import Data.Binary (Binary, get, put)
import Data.Proxy (Proxy (..))
import Data.Sized (Sized, toList, unsafeFromList')
import Data.Word (Word8)
import GHC.TypeNats (KnownNat, Nat, natVal)
import GHC.Exts (IsList (..))
import Utility.Binary (getNBytes)

newtype Bytes n = Bytes (Sized [] n Word8)
	deriving Eq

instance forall (n :: Nat). KnownNat n => IsList (Bytes n) where
	type Item (Bytes n) = Word8
	-- TODO add error checking
	fromList = Bytes . unsafeFromList'
	toList (Bytes xs) = Data.Sized.toList xs

instance forall (n :: Nat). KnownNat n => Show (Bytes n) where
	show = show . GHC.Exts.toList

instance forall (n :: Nat). KnownNat n => Binary (Bytes n) where
	get = Bytes . unsafeFromList' <$> (getNBytes $ natVal $ Proxy @n)
	put = mapM_ put . GHC.Exts.toList