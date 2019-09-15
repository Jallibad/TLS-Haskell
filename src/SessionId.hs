module SessionId where

import Data.Binary
import Utility.Binary (getListWithLengthTag)

data SessionId = SessionId deriving Show

instance Binary SessionId where
	get = getListWithLengthTag @Word8 @Word8 >> pure SessionId
	put = const $ put (0 :: Word8)