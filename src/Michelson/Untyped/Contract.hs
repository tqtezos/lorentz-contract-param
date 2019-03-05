{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson contract in untyped model.

module Michelson.Untyped.Contract
  ( Parameter
  , Storage
  , Contract (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Fmt (genericF)
import Formatting.Buildable (Buildable(build))

import Michelson.Untyped.Type (Type)

type Parameter = Type
type Storage = Type
data Contract op = Contract
  { para :: Parameter
  , stor :: Storage
  , code :: [op]
  } deriving stock (Eq, Show, Functor, Data, Generic)

instance Buildable op => Buildable (Contract op) where
  build = genericF

deriveJSON defaultOptions ''Contract
