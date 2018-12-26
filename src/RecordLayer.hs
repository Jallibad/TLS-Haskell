module RecordLayer where

import Control.Concurrent
import Control.Arrow
import Control.Exception.Safe
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (putLazyByteString)
import Data.ByteString.Lazy as BS hiding (getContents)
-- import Data.Functor
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import UInt

data ProtocolVersion = ProtocolVersion {major :: UInt 8, minor :: UInt 8} deriving (Eq, Show)
instance Binary ProtocolVersion where
	get = ProtocolVersion <$> get <*> get
	put (ProtocolVersion major minor) = put major >> put minor

data ContentType = ChangeCipherSpec | Alert | Handshake | ApplicationData deriving (Eq, Show)
instance Binary ContentType where
	get = getWord8 >>= \case
		20	-> pure ChangeCipherSpec
		21	-> pure Alert
		22	-> pure Handshake
		23	-> pure ApplicationData
		x	-> fail $ mconcat ["Invalid ContentType: ", show x]
	put ChangeCipherSpec = putWord8 20
	put Alert = putWord8 21
	put Handshake = putWord8 22
	put ApplicationData = putWord8 23

data TLSPlaintext = TLSPlaintext {contentType :: ContentType, version :: ProtocolVersion, fragment :: ByteString} deriving (Eq, Show)
instance Binary TLSPlaintext where
	get = do
		contentType <- get
		version <- get
		length <- get :: Get (UInt 16)
		fragment <- getLazyByteString $ fromIntegral length
		pure $ TLSPlaintext contentType version fragment
	put (TLSPlaintext contentType version fragment) = do
		put contentType
		put version
		-- TODO Fragment if too large
		put (fromIntegral $ BS.length fragment :: UInt 16)
		putLazyByteString fragment

receiveFragments :: Socket -> IO [TLSPlaintext]
receiveFragments = (>>= readFragment) . getContents
	where
		readFragment :: ByteString -> IO [TLSPlaintext]
		-- TODO Handle done receiving
		readFragment = runGetOrFail get >>> \case
			(Left _) -> do
				Prelude.putStrLn "Error deserializing Socket contents"
				pure []
			(Right (rest, _, msg)) -> (msg :) <$> readFragment rest

open :: IO Socket
open = do
	let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
	addr:_ <- getAddrInfo (Just hints) Nothing (Just "3000")
	sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
	setSocketOption sock ReuseAddr 1
	bind sock $ addrAddress addr
	listen sock 10
	Prelude.putStrLn $ mconcat ["Listening on port \"", "3000", "\""]
	return sock

server :: IO ()
server = bracket open close $ \sock -> do
	(conn, _) <- accept sock
	void . forkFinally (body conn) . const $ close conn
	where
		body sock = do
			Prelude.putStrLn "Accepted connection from client"
			messages <- receiveFragments sock
			print $ Prelude.head messages

client :: IO ()
client = do
	addr:_ <- getAddrInfo
		(Just $ defaultHints {addrSocketType = Stream})
		(Just "localhost")
		(Just "3000")
	bracket (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> do
		Prelude.putStrLn $ mconcat ["Attempting to connect to \"", "localhost", "\" on port \"", "3000", "\""]
		connect sock $ addrAddress addr
		Prelude.putStrLn "Connected!"
		send sock $ pack [0x14,0x03,0x03,0x00,0x01,0x01]
		send sock $ pack [0x14,0x03,0x03,0x00,0x01,0x01]
		send sock $ pack [0x14,0x03,0x03,0x00,0x01,0x01]
		pure ()