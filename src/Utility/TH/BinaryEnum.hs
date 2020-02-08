{-# LANGUAGE TemplateHaskell #-}

module Utility.TH.BinaryEnum where

-- import Data.Bimap
import Data.Binary
import Data.Traversable
import Language.Haskell.TH

makeBinaryEnumInstance :: forall a b. [(a, b)] -> Q [Dec]
makeBinaryEnumInstance equivalences = do
	-- let context = sequence [[t|Binary b|]]
	let context = pure []
	let binaryInstanceDec = [t|Binary a|]
	(result :: [Dec]) <- pure <$> instanceD context binaryInstanceDec []
	fail "testing"