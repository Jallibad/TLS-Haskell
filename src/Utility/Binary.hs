module Utility.Binary where

import Control.Arrow ((>>>))
import Control.Monad.Loops (unfoldrM, untilM)
import Data.Binary (Binary, Get, Put, decode, get, put)
import Data.Binary.Get (getLazyByteString, isEmpty, runGetOrFail)
import Data.Binary.Put (putLazyByteString, runPut)
import Data.ByteString.Lazy as BS (ByteString, length, null)
import Data.Functor ((<&>))
import GHC.Int (Int64)

getAll :: Binary a => Get [a]
getAll = getAll' get

getAll' :: Get a -> Get [a]
getAll' = flip untilM isEmpty

getNBytes :: Binary a => Int64 -> Get [a]
getNBytes 0 = pure []
getNBytes n = getLazyByteString n >>= either handleError handleSuccess . runGetOrFail getAll
	where
		handleError (_, _, errorMsg) = fail errorMsg
		handleSuccess (_, _, x) = pure x

getListWithLengthTag :: forall n a. (Integral n, Binary n, Binary a) => Get [a]
getListWithLengthTag = get @n >>= getNBytes . fromIntegral

getWithLengthTag :: forall n a. (Integral n, Binary n, Binary a) => Get a
getWithLengthTag = get @n >>= (fromIntegral >>> getLazyByteString) <&> decode

readAll :: Binary a => ByteString -> Either String [a]
readAll = unfoldrM $ \stream ->
	if BS.null stream
	then Right Nothing
	else case runGetOrFail get stream of
		(Left (_rest, _bytesRead, errorMsg)) -> Left errorMsg
		(Right (rest, _bytesRead, msg)) -> Right $ Just (msg, rest)

withLengthTag :: forall n. (Binary n, Num n) => Put -> Put
withLengthTag (runPut -> serialized) = do
	put $ (fromIntegral $ BS.length serialized :: n)
	putLazyByteString serialized