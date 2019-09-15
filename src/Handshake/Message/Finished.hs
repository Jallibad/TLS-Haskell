module Handshake.Message.Finished where

import Data.Binary

data Finished = Finished
	deriving Show

instance Binary Finished where
	get = undefined
	put = undefined