{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson contract in untyped model.

module Michelson.Untyped.Contract
  ( Parameter
  , Storage
  , Contract (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Formatting.Buildable (Buildable(build))
import Text.PrettyPrint.Leijen.Text (nest, semi, (<$$>), (<+>))

import Michelson.Printer.Util (RenderDoc(..), buildRenderDoc, renderOpsList)
import Michelson.Untyped.Type (Type)

type Parameter = Type
type Storage = Type
data Contract op = Contract
  { para :: Parameter
  , stor :: Storage
  , code :: [op]
  } deriving stock (Eq, Show, Functor, Data, Generic)

instance (RenderDoc op) => RenderDoc (Contract op) where
  renderDoc (Contract parameter storage code) =
    "parameter" <+> renderDoc parameter  <> semi <$$>
    "storage"   <+> renderDoc storage    <> semi <$$>
    "code"      <+> nest (length ("code {" :: Text)) (renderOpsList False code <> semi)

instance RenderDoc op => Buildable (Contract op) where
  build = buildRenderDoc

deriveJSON defaultOptions ''Contract
