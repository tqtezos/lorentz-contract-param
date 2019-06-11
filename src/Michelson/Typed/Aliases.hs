module Michelson.Typed.Aliases
  ( Value
  , SomeValue
  , Operation
  ) where

import Michelson.Typed.Instr
import Michelson.Typed.Value

type Value = Value' Instr
type SomeValue = SomeValue' Instr
type Operation = Operation' Instr
