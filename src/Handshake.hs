module Handshake where

import Cipher.CipherSuite
import Control.Monad.State.Lazy
import Handshake.Message
import KeyExchange
import ProtocolVersion
import SessionId
import Utility.Random

newtype Server = Server ()

class MonadState s m => Handshake m s where
	-- sendMessage :: HandshakeMessage -> StateT s m ()
	-- receiveMessage :: StateT s m HandshakeMessage
	-- getRandom :: Random r => StateT s m r
	-- handshake :: 

-- newtype HandshakeTest a = HandshakeTest (State [HandshakeMessage] a)

-- instance Handshake HandshakeTest s where
-- 	sendMessage :: HandshakeMessage -> StateT s HandshakeTest ()
-- 	sendMessage m = undefined
-- 	receiveMessage :: StateT s HandshakeTest HandshakeMessage
-- 	receiveMessage = undefined
-- 	getRandom :: Random r => StateT s m r
-- 	getRandom = undefined

-- server :: forall m. (Monad m, Handshake m Server) => StateT Server m ()
-- server = receiveMessage >>= \case
-- 	ClientHelloMessage (ClientHello _version _random _ _cipherSuites) -> do
-- 		randomBytes <- getRandom
-- 		sendMessage $ ServerHelloMessage $ ServerHello tls1_2 randomBytes SessionId tlsEcdhAnonWithNullSha
-- 		-- sendMessage $ 
-- 	ClientKeyExchangeMessage ClientKeyExchange -> undefined
-- -- 	_ -> undefined