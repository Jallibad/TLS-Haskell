module Handshake.Message.ServerKeyExchange
	( ServerKeyExchange (..)
	) where

import Data.Binary (Binary, get, put)

data ServerKeyExchange = ServerKeyExchange
	deriving Show

instance Binary ServerKeyExchange where
	get = undefined
	put = undefined