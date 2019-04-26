module Michelson.Typed.Aliases
  ( Value
  , Operation
  ) where

import Michelson.Typed.Instr
import Michelson.Typed.Value

type Value = Value' Instr
type Operation = Operation' Instr
