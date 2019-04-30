module Michelson.Let
  ( LetType (..)
  , LetValue (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text as T

import Michelson.Macro (ParsedOp)
import Michelson.Untyped (Type, Value')

-- | A programmer-defined constant
data LetValue = LetValue
  { lvName :: T.Text
  , lvSig :: Type
  , lvVal :: (Value' ParsedOp)
  } deriving (Eq, Show)

-- | A programmer-defined type-synonym
data LetType = LetType
  { ltName :: T.Text
  , ltSig :: Type
  } deriving (Eq, Show)

deriveJSON defaultOptions ''LetValue
deriveJSON defaultOptions ''LetType
