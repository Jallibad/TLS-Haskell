module SecurityParameters where

import Cipher.CipherType
import Utility.Bytes
import Utility.UInt

data ConnectionEnd = Client | Server
	deriving (Show, Enum)

data SecurityParameters = SecurityParameters
	{ connectionEnd :: ConnectionEnd
	-- , prfAlgorithm :: _
	-- , bulkCipherAlgorithm :: _
	, cipherType :: CipherType
	, encKeyLength :: UInt 8
	, blockLength :: UInt 8
	, fixedIvLength :: UInt 8
	, recordIvLength :: UInt 8
	-- , macAlgorithm :: _
	, macLength :: UInt 8
	, macKeyLength :: UInt 8
	-- , compressionAlgorithm :: _
	, masterSecret :: Bytes 48
	, clientRandom :: Bytes 32
	, serverRandom :: Bytes 32
	}