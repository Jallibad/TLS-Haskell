module Handshake.Message
	( module Handshake.Message.ClientHello
	, module Handshake.Message.ClientKeyExchange
	, module Handshake.Message.Finished
	, module Handshake.Message.ServerCertificate
	, module Handshake.Message.ServerHello
	, module Handshake.Message.ServerHelloDone
	, module Handshake.Message.ServerKeyExchange
	, HandshakeMessage (..)
	) where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord8)
-- import Data.ByteString.Lazy (ByteString, unpack)
import Handshake.Message.ClientHello
import Handshake.Message.ClientKeyExchange
import Handshake.Message.Finished
import Handshake.Message.ServerCertificate
import Handshake.Message.ServerHello
import Handshake.Message.ServerHelloDone
import Handshake.Message.ServerKeyExchange
-- import Numeric (showHex)
import Utility.Binary (getWithLengthTag)
import Utility.UInt (UInt)

data HandshakeMessage
	= ClientHelloMessage ClientHello
	| ServerHelloMessage ServerHello
	| ServerCertificateMessage ServerCertificate
	| ServerKeyExchangeMessage ServerKeyExchange
	| ServerHelloDoneMessage ServerHelloDone
	| ClientKeyExchangeMessage ClientKeyExchange
	| FinishedMessage Finished
	deriving Show

instance Binary HandshakeMessage where
	get = getWord8 >>= \case
		-- Constants taken from https://tools.ietf.org/html/rfc5246#appendix-A.4
		0x01 -> getMessageContents ClientHelloMessage
		0x02 -> getMessageContents ServerHelloMessage
		0x0b -> getMessageContents ServerCertificateMessage
		0x0c -> getMessageContents ServerKeyExchangeMessage
		0x0e -> getMessageContents ServerHelloDoneMessage
		0x10 -> getMessageContents ClientKeyExchangeMessage
		0x14 -> getMessageContents FinishedMessage
		x -> fail $ mconcat [show x, " is not a valid handshake message type"]
		where getMessageContents = (<$> getWithLengthTag @(UInt 24))
	put (ClientHelloMessage m)			= put m
	put (ServerHelloMessage m)			= put m
	put (ServerCertificateMessage m)	= put m
	put (ServerKeyExchangeMessage m)	= put m
	put (ServerHelloDoneMessage m)		= put m
	put (ClientKeyExchangeMessage m)	= put m
	put (FinishedMessage m)				= put m

-- showByteString :: ByteString -> String
-- showByteString = Prelude.concatMap (`showHex` "") . unpack