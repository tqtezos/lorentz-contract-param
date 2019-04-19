module Michelson.Printer
  ( RenderDoc(..)
  , printDoc
  , printUntypedContract
  , printTypedContract
  ) where

import Data.Singletons (SingI)
import qualified Data.Text.Lazy as TL

import Michelson.Printer.Util (RenderDoc(..), printDoc)
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U

-- | Convert an untyped contract into a textual representation which
-- will be accepted by the OCaml reference client.
printUntypedContract :: (RenderDoc op) => U.Contract' op -> TL.Text
printUntypedContract = printDoc . renderDoc

-- | Convert a typed contract into a textual representation which
-- will be accepted by the OCaml reference client.
printTypedContract :: (SingI p, SingI s) => T.Contract p s -> TL.Text
printTypedContract = printUntypedContract . T.convertContract
