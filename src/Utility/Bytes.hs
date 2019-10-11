module Utility.Bytes
	( Bytes
	, (++)
	, fromListPadLeft
	, fromListPadRight
	, fromUInt
	, map
	, Utility.Bytes.replicate
	, zipWith
	) where

import Control.Monad ((>=>))
import Data.Binary (Binary, get, put)
import Data.Bits
import Data.Proxy (Proxy (Proxy))
import Data.Sized (Sized, toList, unsafeFromList')
import qualified Data.Sized as Sized ((++), replicate', zipWith)
import Data.Word (Word8)
import GHC.TypeNats (type (*), type (+), KnownNat, Nat, natVal)
import GHC.Exts (IsList (..))
import Prelude hiding ((++), map, zipWith)
import Text.Printf (printf)
import Utility.Binary (getNBytes)
import Utility.UInt (UInt, toWord8Chunk)

newtype Bytes n = Bytes (Sized [] n Word8)
	deriving Eq

fromUInt :: forall (n :: Nat). (KnownNat n, KnownNat (n * 8)) => UInt (n * 8) -> Bytes n
fromUInt = fromList . toWord8Chunk

map :: KnownNat n => (Word8 -> Word8) -> Bytes n -> Bytes n
map f (Bytes xs) = Bytes $ f <$> xs

zipWith :: KnownNat n => (Word8 -> Word8 -> Word8) -> Bytes n -> Bytes n -> Bytes n
zipWith f (Bytes a) (Bytes b) = Bytes $ Sized.zipWith f a b

instance forall (n :: Nat). KnownNat n => IsList (Bytes n) where
	type Item (Bytes n) = Word8
	fromList = fromListPadLeft
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
	get = fromList <$> (getNBytes $ fromIntegral $ natVal $ Proxy @n)
	put = mapM_ put . GHC.Exts.toList

instance KnownNat n => Bits (Bytes n) where
	(.&.) = zipWith (.&.)
	(.|.) = zipWith (.|.)
	complement = map complement
	xor = zipWith xor

(++) :: (KnownNat a, KnownNat b) => Bytes a -> Bytes b -> Bytes (a + b)
(Bytes a) ++ (Bytes b) = Bytes $ a Sized.++ b

replicate :: KnownNat n => Word8 -> Bytes n
replicate = Bytes . Sized.replicate'