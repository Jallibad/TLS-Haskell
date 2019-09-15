module Cipher.CipherSuites
	( CipherSuites
	) where

import Cipher.CipherSuite (CipherSuite)
import Data.Binary (Binary, get, put)
import GHC.Exts (IsList (..))
import Utility.Binary (getListWithLengthTag, withLengthTag)
import Utility.UInt (UInt)

newtype CipherSuites = CipherSuites [CipherSuite]
	deriving newtype (Show, Eq)

type CipherSuitesLengthTag = UInt 16

instance Binary CipherSuites where
	get = CipherSuites <$> getListWithLengthTag @CipherSuitesLengthTag
	put = withLengthTag @CipherSuitesLengthTag . mapM_ put . toList

instance IsList CipherSuites where
	type Item CipherSuites = CipherSuite
	fromList = CipherSuites
	toList (CipherSuites xs) = xs