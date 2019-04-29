{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- |  Custom exceptions that can happen during parsing.

module Michelson.Parser.Error
  ( CustomParserException (..)
  , ParseErrorBundle
  , ParserException (..)
  ) where

import Data.Data (Data(..))
import Fmt (Buildable(build))
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent(..), errorBundlePretty)
import qualified Text.Show (show)

data CustomParserException
  = UnknownTypeException
  | OddNumberBytesException
  | UnexpectedLineBreak
  | ProhibitedLetType Text
  deriving stock (Eq, Data, Ord, Show)

instance ShowErrorComponent CustomParserException where
  showErrorComponent UnknownTypeException = "unknown type"
  showErrorComponent OddNumberBytesException = "odd number bytes"
  showErrorComponent UnexpectedLineBreak = "unexpected linebreak"
  showErrorComponent (ProhibitedLetType t) =
    "prohibited name for type alias in let macros: " <> toString t

data ParserException =
  ParserException (ParseErrorBundle Text CustomParserException)
  deriving (Eq)

instance Show ParserException where
  show (ParserException bundle) = errorBundlePretty bundle

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

instance Buildable ParserException where
  build = build @String . show
