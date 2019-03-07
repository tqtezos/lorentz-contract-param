{-# LANGUAGE DataKinds #-}

-- | Module, providing 'CT' and 'T' data types, representing Michelson
-- language types without annotations.
module Michelson.Typed.T
  ( CT (..)
  , T (..)
  ) where

import Michelson.Untyped.Type (CT(..))

-- | Michelson language type with annotations stripped off.
data T =
    T_c CT
  | T_key
  | T_unit
  | T_signature
  | T_option T
  | T_list T
  | T_set CT
  | T_operation
  | T_contract T
  | T_pair T T
  | T_or T T
  | T_lambda T T
  | T_map CT T
  | T_big_map CT T
  deriving (Eq, Show)
