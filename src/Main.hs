module Main where

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import Prelude hiding (replicate)

main :: IO ()
main = print ("Hello World" :: String)

type Parser e = Parsec e String

-- class Expression lexeme where
-- 	parser :: Parser e lexeme

data Expression = Constant Integer | Function (Expression -> Expression) | Error String | Application Expression Expression

newtype Identifier = Identifier String

-- type Scope = Map FunctionName

lexeme :: Parser () a -> Parser () a
lexeme = L.lexeme $ L.space space1 lineComment blockComment
	where
		lineComment = L.skipLineComment "//"
		blockComment = L.skipBlockComment "/*" "*/"

integer :: Parser () Integer
integer = lexeme L.decimal

constant :: Parser () Expression
constant = Constant <$> integer

-- expression :: Parser Expression
-- expression =
-- 	fmap Constant integer <|>
-- 	-- fmap 