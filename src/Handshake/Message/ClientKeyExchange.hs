module Handshake.Message.ClientKeyExchange
	( ClientKeyExchange (..)
	) where

import Data.Binary (Binary, get, put)

data ClientKeyExchange = ClientKeyExchange
	deriving Show

instance Binary ClientKeyExchange where
	get = undefined
	put = undefined