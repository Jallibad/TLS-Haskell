module CompressionMethod where

import Data.Binary
import RecordLayer.Types

class CompressionMethod c m where
	state :: a
	compress :: Monad m => m a -> TLSPlaintext -> m TLSCompressedtext
	-- TODO Report decompression length exceeds 2^14
	decompress :: Monad m => m a -> TLSCiphertext -> m TLSCompressedtext