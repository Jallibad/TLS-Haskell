module Play where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser e = Parsec e String

sc :: Parser () ()
sc = L.space space1 lineComment blockComment
	where
		lineComment = L.skipLineComment "//"
		blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser () a -> Parser () a
lexeme = L.lexeme sc

integer :: Parser () Integer
integer = lexeme L.decimal

symbol :: String -> Parser () String
symbol = L.symbol sc

expression :: Parser () Integer
expression = makeExprParser integer operators

operators :: [[Operator (Parser ()) Integer]]
operators = [[Prefix $ negate <$ symbol "-"]]

-- operator :: Parser () Operator
-- operator = chunk "*"