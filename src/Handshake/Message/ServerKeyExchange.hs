{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Handshake.Message.ServerKeyExchange
	( ServerKeyExchange (..)
	) where

import Data.Binary (Binary, get, put)
import Data.Functor ((<&>))
import KeyExchange
import Utility.TH.BinaryEnum (makeBinaryEnumInstance)
import Utility.UInt

data CurveType =
	NamedCurve

$(makeBinaryEnumInstance
	[ (NamedCurve, 0x03 :: UInt 32)
	])

-- instance Binary CurveType where
-- 	get = get @(UInt 32) <&> \case
-- 		0x03 -> NamedCurve
-- 	put NamedCurve = put (0x03 :: UInt 32)

data ServerKeyExchange a where
	-- DiffieHellman :: 
	ServerKeyExchange :: PublicKey a -> ServerKeyExchange a

deriving instance Show (PublicKey a) => Show (ServerKeyExchange a)

instance Binary (PublicKey a) => Binary (ServerKeyExchange a) where
	get = undefined
	put = undefined