module Michelson.Parser.Helpers
  ( mkParser
  , sepEndBy1
  , sepBy2
  , parseDef
  ) where

import Data.Default (Default(..))
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as P

import Michelson.Parser.Lexer (symbol')
import Michelson.Parser.Types (Parser)

sepEndBy1 :: MonadPlus m => m a -> m sep -> m (NonEmpty a)
sepEndBy1 = fmap NE.fromList ... P.sepEndBy1

-- | @endBy2 p sep@ parses two or more occurrences of @p@, separated by @sep@.
sepBy2 :: MonadPlus m => m a -> m sep -> m (NonEmpty a)
sepBy2 parser sep = do
  e <- parser
  void sep
  es <- P.sepBy1 parser sep
  return $ e :| es

-- | Make a parser from a string
mkParser :: (a -> Text) -> a -> Parser a
mkParser f a = (P.try $ symbol' (f a)) >> return a

-- | Apply given parser and return default value if it fails.
parseDef :: Default a => Parser a -> Parser a
parseDef a = P.try a <|> pure def
