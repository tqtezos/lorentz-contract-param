{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Morley.Lexer (
    lexeme
  , mSpace
  , symbol
  , symbol'
  , string'
  , parens
  , braces
  , brackets
  , semicolon
  , comma
  ) where

import Morley.Types (Parser)

import Data.Char (toLower)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string')
import qualified Text.Megaparsec.Char.Lexer as L

-- Lexing
lexeme :: Parser a -> Parser a
lexeme = L.lexeme mSpace
mSpace = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol = L.symbol mSpace
symbol' str = symbol str <|> symbol (T.map toLower str)
string' str = string str <|> string (T.map toLower str)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semicolon = symbol ";"
comma = symbol ","
