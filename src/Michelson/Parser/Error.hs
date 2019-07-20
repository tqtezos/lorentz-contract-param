{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- |  Custom exceptions that can happen during parsing.

module Michelson.Parser.Error
  ( CustomParserException (..)
  , StringLiteralParserException (..)
  , ParseErrorBundle
  , ParserException (..)
  ) where

import Data.Data (Data(..))
import Fmt (Buildable(build), (+|), (|+))
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent(..), errorBundlePretty)
import qualified Text.Show (show)

import Util.Instances ()
import Util.Named ()
import Util.Positive

data CustomParserException
  = UnknownTypeException
  | StringLiteralException StringLiteralParserException
  | OddNumberBytesException
  | ProhibitedLetType Text
  | WrongTagArgs Natural Positive
  | WrongAccessArgs Natural Positive
  | WrongSetArgs Natural Positive
  deriving stock (Eq, Data, Ord, Show)

instance ShowErrorComponent CustomParserException where
  showErrorComponent UnknownTypeException = "unknown type"
  showErrorComponent (StringLiteralException e) = showErrorComponent e
  showErrorComponent OddNumberBytesException = "odd number bytes"
  showErrorComponent (ProhibitedLetType t) =
    "prohibited name for type alias in let macros: " <> toString t
  showErrorComponent (WrongTagArgs idx size) =
    "TAG: too large index: " +| idx |+ " \
           \exceedes union size " +| size |+ ""
  showErrorComponent (WrongAccessArgs idx size) =
    "ACCESS: too large index: " +| idx |+ " \
           \exceedes tuple size " +| size |+ ""
  showErrorComponent (WrongSetArgs idx size) =
    "SET: too large index: " +| idx |+ " \
           \exceedes tuple size " +| size |+ ""

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
