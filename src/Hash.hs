module Hash where

import Data.Binary (encode)
import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS (length, unpack)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (type (+), type (-), KnownNat, natVal)
import Prelude hiding ((++), map, replicate)
import Utility.Bytes (Bytes, (++), fromListPadRight, map, replicate)

class (KnownNat b, KnownNat n, (n + (b - n)) ~ b, KnownNat (b - n)) =>
	HashFunction h b n | h -> b n where
		hash :: ByteString -> Bytes n

hmac :: forall h b n. HashFunction h b n => ByteString -> ByteString -> Bytes n
hmac (hashIfNecessary @h @b @n -> k) = hash @h . mappend opad . encode . hash @h . mappend ipad
	where
		opad = encode $ map (xor 0x5c) k
		ipad = encode $ map (xor 0x36) k

hashIfNecessary :: forall h b n. HashFunction h b n => ByteString -> Bytes b
hashIfNecessary x = if BS.length x > fromIntegral (natVal $ Proxy @b)
	then hash @h x ++ (replicate 0 :: Bytes (b - n))
	else fromListPadRight $ BS.unpack x