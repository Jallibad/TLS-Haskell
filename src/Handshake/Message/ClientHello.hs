module Handshake.Message.ClientHello where

import Cipher.CipherSuites (CipherSuites)
import Data.Binary (Binary, get, put)
import ProtocolVersion (ProtocolVersion)
import SessionId (SessionId)
import Utility.Bytes (Bytes)

data ClientHello = ClientHello	{ clientVersion :: ProtocolVersion
								, random :: Bytes 1--32
								, sessionId :: SessionId
								, cipherSuites :: CipherSuites
								-- TODO add compression methods
								-- TODO add extensions
								}
	deriving Show

instance Binary ClientHello where
	get = ClientHello <$> get <*> get <*> get <*> get
	put (ClientHello version random sessionId ciphers) = do
		put version
		put random
		put sessionId
		put ciphers