module Michelson.Printer
  ( RenderDoc(..)
  , printDoc
  , printUntypedContract
  ) where

import qualified Data.Text.Lazy as TL

import Michelson.Printer.Util (RenderDoc(..), printDoc)
import qualified Michelson.Untyped as Un

-- | Convert an untyped contract into a textual representation which
-- will be accepted by the OCaml reference client.
printUntypedContract :: (RenderDoc op) => Un.Contract op -> TL.Text
printUntypedContract = printDoc . renderDoc
