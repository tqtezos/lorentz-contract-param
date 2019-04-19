module Michelson.Lexer
  ( lexeme
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
  , varID
  ) where

import Data.Char (isDigit, isLower, toLower)
import qualified Data.Text as T
import Text.Megaparsec (MonadParsec, Tokens, between, satisfy)
import Text.Megaparsec.Char (lowerChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Michelson.Types (Parser)
import qualified Michelson.Untyped as U

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

varID :: Parser U.Var
varID = lexeme $ do
  v <- lowerChar
  vs <- many lowerAlphaNumChar
  return $ U.Var (toText (v:vs))
  where
    lowerAlphaNumChar :: Parser Char
    lowerAlphaNumChar = satisfy (\x -> isLower x || isDigit x)
