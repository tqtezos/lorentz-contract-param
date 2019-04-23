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
  deriving stock (Eq, Data, Ord, Show)

instance ShowErrorComponent CustomParserException where
  showErrorComponent UnknownTypeException = "unknown type"
  showErrorComponent OddNumberBytesException = "odd number bytes"
  showErrorComponent UnexpectedLineBreak = "unexpected linebreak"

data ParserException =
  ParserException (ParseErrorBundle Text CustomParserException)
  deriving (Eq)

instance Show ParserException where
  show (ParserException bundle) = errorBundlePretty bundle

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

instance Buildable ParserException where
  build = build @String . show
