module Michelson.TypeCheck
  ( typeCheckContract
  , typeCheckValue
  , typeCheckList
  , typeCheckCValue
  , module E
  , module M
  , module T
  , eqType
  , compareTypes
  ) where

import Michelson.TypeCheck.Error as E
import Michelson.TypeCheck.Instr
import Michelson.TypeCheck.TypeCheck as T
import Michelson.TypeCheck.Types as M
import Michelson.TypeCheck.Value

import Michelson.TypeCheck.Helpers (eqType, compareTypes)
