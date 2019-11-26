module RecordLayer.Network where

import Control.Arrow ((>>>))
import Control.Exception (bracket)
import Control.Monad ((>=>), void)
import Data.ByteString.Lazy as BS (ByteString, pack)
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy (getContents, send)
import Prelude hiding (getContents)
import RecordLayer.Types
import Utility.Binary

receiveFragments :: Socket -> IO [TLSPlaintext]
receiveFragments = (pure readAll <*>) . getContents

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
server = bracket open close $ accept >=> fst >>> \sock -> do
	Prelude.putStrLn "Accepted connection from client"
	m1 : messages <- receiveFragments sock
	print m1
	send sock sampleMessage
	print messages
	close sock

client :: IO ()
client = do
	addr:_ <- getAddrInfo
		(Just $ defaultHints {addrSocketType = Stream})
		(Just "localhost")
		(Just "3000")
	bracket (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> void $ do
		Prelude.putStrLn $ mconcat ["Attempting to connect to \"", "localhost", "\" on port \"", "3000", "\""]
		connect sock $ addrAddress addr
		Prelude.putStrLn "Connected!"
		messages <- receiveFragments sock
		send sock sampleMessage
		print messages
		send sock sampleMessage
		send sock sampleMessage

sampleMessage :: ByteString
sampleMessage = pack [0x14,0x03,0x03,0x00,0x05,49,50,51,52,53]