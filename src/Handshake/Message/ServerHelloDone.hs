module Handshake.Message.ServerHelloDone
	( ServerHelloDone (..)
	) where

import Data.Binary (Binary, get, put)

data ServerHelloDone = ServerHelloDone
	deriving Show

instance Binary ServerHelloDone where
	get = pure ServerHelloDone
	put = const $ pure ()