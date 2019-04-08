module Michelson.TypeCheck
  ( typeCheckContract
  , typeCheckValue
  , typeCheckList
  , typeCheckCValue
  , module M
  , eqT'
  ) where

import Michelson.TypeCheck.Instr
import Michelson.TypeCheck.Types as M
import Michelson.TypeCheck.Value

import Michelson.TypeCheck.Helpers (eqT')
