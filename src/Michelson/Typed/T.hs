{-# LANGUAGE DataKinds #-}

-- | Module, providing 'CT' and 'T' data types, representing Michelson
-- language types without annotations.
module Michelson.Typed.T
  ( CT (..)
  , T (..)
  , ToCT
  , ToT
  ) where

import Michelson.Untyped.Type (CT(..), ToCT)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

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

-- | Type function that converts a regular Haskell type into a @T@ type.
-- TODO: what should be done with 'TBigMap'?
type family ToT t :: T where
  ToT Integer = 'Tc (ToCT Integer)
  ToT Int = 'Tc (ToCT Int)
  ToT Natural = 'Tc (ToCT Natural)
  ToT Word64 = 'Tc (ToCT Word64)
  ToT Text = 'Tc (ToCT Text)
  ToT Bool = 'Tc (ToCT Bool)
  ToT ByteString = 'Tc (ToCT ByteString)
  ToT Mutez = 'Tc (ToCT Mutez)
  ToT Address = 'Tc (ToCT Address)
  ToT KeyHash = 'Tc (ToCT KeyHash)
  ToT Timestamp = 'Tc (ToCT Timestamp)

  ToT () = 'TUnit
  ToT (a, b) = 'TPair (ToT a) (ToT b)
  ToT [a] = 'TList (ToT a)
  ToT (Maybe a) = 'TOption (ToT a)
  ToT (Either a b) = 'TOr (ToT a) (ToT b)
  ToT (Set k) = 'TSet (ToCT k)
  ToT (Map k v) = 'TMap (ToCT k) (ToT v)
  ToT PublicKey = 'TKey
  ToT Signature = 'TSignature
