module Handshake.Message.ServerCertificate where

import Data.Binary

data ServerCertificate = ServerCertificate
	deriving Show

instance Binary ServerCertificate where
	get = undefined
	put = undefined