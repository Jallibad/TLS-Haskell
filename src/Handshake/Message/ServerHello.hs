module Handshake.Message.ServerHello where

import Cipher.CipherSuite (CipherSuite)
import Data.Binary (Binary, get, put)
import ProtocolVersion (ProtocolVersion)
import SessionId (SessionId)
import Utility.Bytes (Bytes)

data ServerHello = ServerHello	{ serverVersion :: ProtocolVersion
								, random :: Bytes 32
								, sessionId :: SessionId
								, cipherSuite :: CipherSuite
								-- TODO add compression methods
								-- TODO add extensions
								}
	deriving Show

instance Binary ServerHello where
	get = ServerHello <$> get <*> get <*> get <*> get
	put (ServerHello version random sessionId ciphers) = do
		put version
		put random
		put sessionId
		put ciphers