module Michelson.Printer
  ( RenderDoc(..)
  , printDoc
  , printUntypedContract
  , printTypedContract
  , printTypedValue
  ) where

import Data.Singletons (SingI)
import qualified Data.Text.Lazy as TL

import Michelson.Printer.Util (RenderDoc(..), printDoc)
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U

-- | Convert an untyped contract into a textual representation which
-- will be accepted by the OCaml reference client.
printUntypedContract :: (RenderDoc op) => Bool -> U.Contract' op -> TL.Text
printUntypedContract forceSingleLine = printDoc forceSingleLine . renderDoc

-- | Convert a typed contract into a textual representation which
-- will be accepted by the OCaml reference client.
printTypedContract :: (SingI p, SingI s) => Bool -> T.Contract p s -> TL.Text
printTypedContract forceSingleLine = printUntypedContract forceSingleLine . T.convertContract

printTypedValue :: (SingI t, T.HasNoOp t) => Bool -> T.Value t -> TL.Text
printTypedValue forceSingleLine = printDoc forceSingleLine . renderDoc . T.untypeValue
