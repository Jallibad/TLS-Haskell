module Utility.Bytes
	( Bytes
	, (++)
	, pattern (:<|)
	, pattern (:|>)
	, pattern Empty
	, fromListPadLeft
	, fromListPadRight
	, map
	, Utility.Bytes.replicate
	, zipWith
	, unsafeFromList
	) where

import Control.Arrow (second)
import Control.Monad ((>=>))
import Data.Binary (Binary, get, put)
import Data.Bits
import Data.ListLike (ListLike, toList)
import Data.Proxy (Proxy (Proxy))
import Data.Singletons.Prelude.Enum (Succ)
import Data.Sized (Sized, fromListWithDefault', toList, uncons, unsafeFromList')
import qualified Data.Sized as Sized ((++), replicate', zipWith)
import Data.Word (Word8)
import GHC.Exts (IsList (..))
import GHC.TypeNats (type (+), KnownNat, Nat, natVal)
import Prelude hiding ((++), map, zipWith)
import System.Random
import Text.Printf (printf)
import Utility.Binary (getNBytes)

newtype Bytes n = Bytes {unbytes :: Sized [] n Word8}
	deriving Eq

map :: KnownNat n => (Word8 -> Word8) -> Bytes n -> Bytes n
map f (Bytes xs) = Bytes $ f <$> xs

zipWith :: KnownNat n => (Word8 -> Word8 -> Word8) -> Bytes n -> Bytes n -> Bytes n
zipWith f (Bytes a) (Bytes b) = Bytes $ Sized.zipWith f a b

instance forall (n :: Nat). KnownNat n => IsList (Bytes n) where
	type Item (Bytes n) = Word8
	fromList = Bytes . fromListWithDefault' 0
	toList (Bytes xs) = Data.Sized.toList xs

fromListPadLeft :: forall (n :: Nat). KnownNat n => [Word8] -> Bytes n
fromListPadLeft xs = Bytes $ unsafeFromList' $ if length xs <= byteLen
	then Prelude.replicate (byteLen - length xs) 0 <> xs
	else error $ mconcat ["Expected ", show byteLen, " bytes, ", show $ length xs, " bytes provided"]
	where byteLen = fromIntegral $ natVal $ Proxy @n

fromListPadRight :: forall (n :: Nat). KnownNat n => [Word8] -> Bytes n
fromListPadRight xs = Bytes $ unsafeFromList' $ if length xs <= byteLen
	then xs <> Prelude.replicate (byteLen - length xs) 0
	else error $ mconcat ["Expected ", show byteLen, " bytes, ", show $ length xs, " bytes provided"]
	where byteLen = fromIntegral $ natVal $ Proxy @n

instance forall (n :: Nat). KnownNat n => Show (Bytes n) where
	show = GHC.Exts.toList >=> printf "%02x"

instance forall (n :: Nat). KnownNat n => Binary (Bytes n) where
	get = fromList <$> getNBytes (fromIntegral $ natVal $ Proxy @n)
	put = mapM_ put . GHC.Exts.toList

instance KnownNat n => Bits (Bytes n) where
	(.&.) = zipWith (.&.)
	(.|.) = zipWith (.|.)
	complement = map complement
	xor = zipWith xor

(++) :: (KnownNat a, KnownNat b) => Bytes a -> Bytes b -> Bytes (a + b)
(Bytes a) ++ (Bytes b) = Bytes $ a Sized.++ b

{-# COMPLETE Empty #-}
pattern Empty :: Bytes 0
pattern Empty <- []
	where Empty = []

{-# COMPLETE (:<|) #-}
pattern (:<|) :: (KnownNat a, (a + 1) ~ (1 + a)) => Word8 -> Bytes a -> Bytes (Succ a)
pattern x :<| xs <- (second Bytes . uncons . unbytes -> (x, xs))
	where x :<| xs = [x] ++ xs

{-# COMPLETE (:|>) #-}
pattern (:|>) :: KnownNat a => Bytes a -> Word8 -> Bytes (Succ a)
pattern xs :|> x <- (second Bytes . uncons . unbytes -> (x, xs))
	where xs :|> x = xs ++ [x]

replicate :: KnownNat n => Word8 -> Bytes n
replicate = Bytes . Sized.replicate'

-- randomBytes :: 

instance KnownNat n => Random (Bytes n) where
	randomR = undefined
	random = randomR (Utility.Bytes.replicate 0, Utility.Bytes.replicate 0xff)

unsafeFromList :: (KnownNat n, ListLike (f Word8) Word8) => f Word8 -> Bytes n
unsafeFromList = Bytes . unsafeFromList' . Data.ListLike.toList