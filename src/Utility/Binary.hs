module Utility.Binary where

import Control.Arrow
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Loops
import Data.Binary (Binary, get)
import Data.Binary.Get-- (Get, runGetOrFail)
import Data.ByteString.Lazy as BS (ByteString, null)
import Data.Function
import Data.Functor
import Data.List (unfoldr)
import RecordLayer.Types (MalformedMessageException (..))

getAll :: Binary a => Get [a]
getAll = untilM get isEmpty

getNBytes :: forall a b. (Integral a, Binary b) => a -> Get [b]
getNBytes = getLazyByteString . fromIntegral >=> thing
	where
		thing :: ByteString -> Get [b]
		thing = either handleError handleSuccess . runGetOrFail getAll
		handleError (_, _, errorMsg) = fail errorMsg
		handleSuccess (_, _, x) = pure x

getWithLengthTag :: forall a b. (Integral a, Binary a, Binary b) => Get [b]
getWithLengthTag = get @a >>= getNBytes

readAll :: Binary a => ByteString -> [a]
readAll = unfoldr $ \stream ->
	if BS.null stream then Nothing
	else case runGetOrFail get stream of
		(Left (_rest, _bytesRead, errorMsg)) -> throw $ MalformedMessageException errorMsg
		(Right (rest, _bytesRead, msg)) -> Just (msg, rest)