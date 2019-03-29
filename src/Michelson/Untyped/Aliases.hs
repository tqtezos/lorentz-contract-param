-- | Some simple aliases for Michelson types.

module Michelson.Untyped.Aliases
  ( UntypedContract
  , UntypedValue
  ) where

import qualified Michelson.Untyped.Instr as Untyped
import qualified Michelson.Untyped.Value as Untyped
import qualified Michelson.Untyped.Contract as Untyped

type UntypedValue = Untyped.Value Untyped.ExpandedOp
type UntypedContract = Untyped.Contract Untyped.ExpandedOp
