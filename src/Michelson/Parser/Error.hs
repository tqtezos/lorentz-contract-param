{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- |  Custom exceptions that can happen during parsing.

module Michelson.Parser.Error
  ( CustomParserException (..)
  , StringLiteralParserException (..)
  , ParseErrorBundle
  , ParserException (..)
  ) where

import Data.Data (Data(..))
import Fmt (Buildable(build))
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent(..), errorBundlePretty)
import qualified Text.Show (show)

data CustomParserException
  = UnknownTypeException
  | StringLiteralException StringLiteralParserException
  | OddNumberBytesException
  | ProhibitedLetType Text
  deriving stock (Eq, Data, Ord, Show)

instance ShowErrorComponent CustomParserException where
  showErrorComponent UnknownTypeException = "unknown type"
  showErrorComponent (StringLiteralException e) = showErrorComponent e
  showErrorComponent OddNumberBytesException = "odd number bytes"
  showErrorComponent (ProhibitedLetType t) =
    "prohibited name for type alias in let macros: " <> toString t

data StringLiteralParserException
  = InvalidEscapeSequence Char
  | InvalidChar Char
  deriving stock (Eq, Data, Ord, Show)

instance ShowErrorComponent StringLiteralParserException where
  showErrorComponent (InvalidEscapeSequence c) =
    "invalid escape sequence '\\" <> [c] <> "'"
  showErrorComponent (InvalidChar c) =
    "invalid character '" <> [c] <> "'"

data ParserException =
  ParserException (ParseErrorBundle Text CustomParserException)
  deriving (Eq)

instance Show ParserException where
  show (ParserException bundle) = errorBundlePretty bundle

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

instance Buildable ParserException where
  build = build @String . show
