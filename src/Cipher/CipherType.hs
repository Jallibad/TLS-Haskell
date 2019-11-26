module Cipher.CipherType where

data CipherType = Stream | Block | Aead
	deriving (Enum, Show)