module ProtocolVersion where

import Data.Binary
import Utility.UInt (UInt)

data ProtocolVersion = ProtocolVersion {major :: UInt 8, minor :: UInt 8} deriving (Eq, Show)
instance Binary ProtocolVersion where
	get = ProtocolVersion <$> get <*> get
	put (ProtocolVersion major minor) = put major >> put minor

tls1_2 :: ProtocolVersion
tls1_2 = ProtocolVersion 0x03 0x03	