module CompressionMethod where

-- import Data.Binary
import Data.Void
import RecordLayer.Types

class CompressionMethod c where
	type State c
	compress :: TLSPlaintext -> Maybe TLSCompressedtext
	-- TODO Report decompression length exceeds 2^14
	decompress :: TLSCiphertext -> Maybe TLSCompressedtext

data Null

instance CompressionMethod Null where
	type State Null = Void
	-- compress 