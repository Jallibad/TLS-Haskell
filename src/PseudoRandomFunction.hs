module PseudoRandomFunction where

import Data.Binary (encode)
import Data.ByteString.Lazy (ByteString, unpack)
import GHC.Exts (IsList (fromList))
import GHC.TypeNats (KnownNat)
import Hash
import Utility.Bytes (Bytes)

prf :: forall h n. (HashFunction h, KnownNat n) => ByteString -> ByteString -> ByteString -> Bytes n
prf secret label seed = pHash @h @n secret $ label <> seed

pHash :: forall h n. (HashFunction h, KnownNat n) => ByteString -> ByteString -> Bytes n
pHash secret seed = fromList $ unpack $ mconcat $ encode . h . (<> seed) <$> iterate (encode . h) seed
	where
		h = hmac @h secret

-- pHash :: ByteString -> ByteString -> 
a :: forall h. HashFunction h => ByteString -> ByteString -> [ByteString]
a = iterate . (encode .) . hmac @h