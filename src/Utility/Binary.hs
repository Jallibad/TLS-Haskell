module Utility.Binary where

import Control.Exception (throw)
import Control.Monad.Loops (untilM)
import Data.Binary (Binary, Get, Put, decode, get, put)
import Data.Binary.Get (getLazyByteString, isEmpty, runGetOrFail)
import Data.Binary.Put (putLazyByteString, runPut)
import Data.ByteString.Lazy as BS (ByteString, length, null)
import Data.List (unfoldr)
import GHC.Int (Int64)
import RecordLayer.Types (MalformedMessageException (..))

getAll :: Binary a => Get [a]
getAll = getAll' get

getAll' :: Get a -> Get [a]
getAll' = flip untilM isEmpty

getNBytes :: Binary a => Int64 -> Get [a]
getNBytes 0 = pure []
getNBytes n = getLazyByteString n >>=
				either handleError handleSuccess . runGetOrFail getAll
	where
		handleError (_, _, errorMsg) = fail errorMsg
		handleSuccess (_, _, x) = pure x

getListWithLengthTag :: forall n a. (Integral n, Binary n, Binary a) => Get [a]
getListWithLengthTag = get @n >>= getNBytes . fromIntegral

getWithLengthTag :: forall n a. (Integral n, Binary n, Binary a) => Get a
getWithLengthTag = do
	len <- get @n
	decode <$> getLazyByteString (fromIntegral len)

readAll :: Binary a => ByteString -> [a]
readAll = unfoldr $ \stream ->
	if BS.null stream
		then Nothing
		else case runGetOrFail get stream of
			(Left (_rest, _bytesRead, errorMsg)) -> throw $ MalformedMessageException errorMsg
			(Right (rest, _bytesRead, msg)) -> Just (msg, rest)

withLengthTag :: forall n. (Binary n, Num n) => Put -> Put
withLengthTag (runPut -> serialized) = do
	put $ (fromIntegral $ BS.length serialized :: n)
	putLazyByteString serialized