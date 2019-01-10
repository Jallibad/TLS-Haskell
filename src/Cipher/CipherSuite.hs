module Cipher.CipherSuite where

import Control.Arrow ((>>>))
import Data.Bimap
import Data.Binary
import Data.Function (on)
import Data.Maybe
import Data.Ord (comparing)
import Prelude hiding (lookup)
import Utility.UInt

-- TODO Allow equality and comparison for Unsupported
data CipherSuite = CipherSuite {name :: String} | Unsupported
instance Eq CipherSuite where
	(==) = (==) `on` name
instance Ord CipherSuite where
	compare = comparing name
instance Binary CipherSuite where
	get = fromMaybe Unsupported . flip lookup cipherSuites <$> ((,) <$> get <*> get)
	put = flip lookupR cipherSuites >>> \case
				Just (c1, c2) -> put c1 >> put c2
				Nothing -> fail "Can't serialize unsupported"

cipherSuites :: Bimap (UInt 8, UInt 8) CipherSuite
cipherSuites = fromList [ ((0x00, 0x00), tls_null_with_null_null)
						]

tls_null_with_null_null :: CipherSuite
tls_null_with_null_null = CipherSuite "TLS_NULL_WITH_NULL_NULL"