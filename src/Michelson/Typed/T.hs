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
    Tc CT
  | TKey
  | TUnit
  | TSignature
  | TOption T
  | TList T
  | TSet CT
  | TOperation
  | TContract T
  | TPair T T
  | TOr T T
  | TLambda T T
  | TMap CT T
  | TBigMap CT T
  deriving (Eq, Show)
