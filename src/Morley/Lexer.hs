module Morley.Lexer (
    lexeme
  , mSpace
  , symbol
  , symbol'
  , string'
  , parens
  , braces
  , brackets
  , brackets'
  , semicolon
  , comma
  ) where

import Morley.Types (Parser)

import Data.Char (toLower)
import qualified Data.Text as T
import Text.Megaparsec (between, MonadParsec, Tokens)
import Text.Megaparsec.Char (space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Lexing
lexeme :: Parser a -> Parser a
lexeme = L.lexeme mSpace

mSpace :: Parser ()
mSpace = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol :: Tokens Text -> Parser (Tokens Text)
symbol = L.symbol mSpace

symbol' :: Text -> Parser (Tokens Text)
symbol' str = symbol str <|> symbol (T.map toLower str)

string' :: (MonadParsec e s f, Tokens s ~ Text) => Text -> f Text
string' str = string str <|> string (T.map toLower str)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

brackets' :: Parser a -> Parser a
brackets' = between (string "[") (string "]")

semicolon :: Parser (Tokens Text)
semicolon = symbol ";"

comma :: Parser (Tokens Text)
comma = symbol ","
