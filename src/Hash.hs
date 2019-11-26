module Hash where

import Control.Arrow
import Data.Binary (encode)
import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS (length, unpack)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (type (+), type (-), KnownNat, Nat, natVal)
import Prelude hiding ((++), map, replicate)
import Utility.Bytes (Bytes, (++), fromListPadRight, map, replicate)

class
	( KnownNat (BlockSize h)
	, KnownNat (OutputSize h)
	, KnownNat (BlockSize h - OutputSize h)
	, (OutputSize h + (BlockSize h - OutputSize h)) ~ BlockSize h)
	=> HashFunction (h :: Type) where
		type BlockSize h :: Nat
		type OutputSize h :: Nat
		hash :: ByteString -> Bytes (OutputSize h)

hmac :: forall h. HashFunction h =>
	ByteString -> ByteString -> Bytes (OutputSize h)
hmac = uncurry (.) . (outer &&& inner) . hashIfNecessary
	where
		outer = (hash @h .) . mappend . encode . map (xor 0x5c)
		inner = ((encode . hash @h) .) . mappend . encode . map (xor 0x36)
		hashIfNecessary :: ByteString -> Bytes (BlockSize h)
		hashIfNecessary x = if BS.length x > fromIntegral (natVal $ Proxy @(BlockSize h))
			then hash @h x ++ (replicate 0 :: Bytes ((BlockSize h) - (OutputSize h)))
			else fromListPadRight $ BS.unpack x