module Michelson.Parser.Helpers
  ( mkParser
  , sepEndBy1
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as P

import Michelson.Lexer (symbol')
import Michelson.Parser.Types (Parser)

sepEndBy1 :: MonadPlus m => m a -> m sep -> m (NonEmpty a)
sepEndBy1 = fmap NE.fromList ... P.sepEndBy1

-- | Make a parser from a string
mkParser :: (a -> Text) -> a -> Parser a
mkParser f a = (P.try $ symbol' (f a)) >> return a
