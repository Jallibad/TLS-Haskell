module Handshake.Types where

import Data.Word (Word8)
import Utility.BinaryEnum

data HandshakeType	= HelloRequest
					| ClientHello
					| ServerHello
					| Certificate
					| ServerKeyExchange
					| CertificateRequest
					| ServerHelloDone
					| CertificateVerify
					| ClientKeyExchange
					| Finished
					deriving (Show, Eq, Ord, Enum)
instance BinaryEnum HandshakeType Word8 where
	equivalences = createEquivalences [0, 1, 2, 11, 12, 13, 14, 15, 16, 20]