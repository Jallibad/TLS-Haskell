module RecordLayer.Types where

import Control.Exception (Exception)
import Data.Bimap
import Data.Binary
-- import Data.Binary (Binary, Get, Put, Word8, get, getWord8, put, putWord8)
import Data.Binary.Get (getLazyByteString)
import Data.Binary.Put (putLazyByteString)
import Data.ByteString.Lazy as BS
import GHC.TypeNats (type (*), KnownNat, Nat)
import ProtocolVersion
import Utility.BinaryEnum (BinaryEnum, equivalences)
import Utility.UInt (UInt)

data MalformedMessageException = MalformedMessageException String
	deriving Show
instance Exception MalformedMessageException

data ContentType = ChangeCipherSpec | Alert | Handshake | ApplicationData
	deriving (Eq, Ord, Show)
instance BinaryEnum ContentType Word8 where
	equivalences = fromList [(ChangeCipherSpec, 20), (Alert, 21), (Handshake, 22), (ApplicationData, 23)]

getByteStringWithLength :: forall (n :: Nat). KnownNat n => Get ByteString
getByteStringWithLength = fromIntegral <$> get @(UInt (n * 8)) >>= getLazyByteString

makeByteStringWithLengths :: forall (n :: Nat). KnownNat n => ByteString -> [(UInt n, ByteString)]
makeByteStringWithLengths = undefined

data TLSPlaintext = TLSPlaintext {contentType :: ContentType, version :: ProtocolVersion, fragment :: ByteString}
	deriving (Eq, Show)
instance Binary TLSPlaintext where
	get = TLSPlaintext <$> get <*> get <*> getByteStringWithLength @16
	put (TLSPlaintext contentType version fragment) = do
		put contentType
		put version
		-- TODO Fragment if too large
		put (fromIntegral $ BS.length fragment :: UInt 16)
		putLazyByteString fragment

data TLSCompressedtext
instance Binary TLSCompressedtext where
	get = undefined
	put = undefined

data TLSCiphertext
instance Binary TLSCiphertext where
	get = undefined
	put = undefined