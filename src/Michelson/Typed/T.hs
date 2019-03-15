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

-- | Type function that converts a regular Haskell type into a @T@ type.
-- TODO: what should be done with 'T_big_map'?
type family ToT t :: T where
  ToT Integer = 'T_c (ToCT Integer)
  ToT Int = 'T_c (ToCT Int)
  ToT Natural = 'T_c (ToCT Natural)
  ToT Word64 = 'T_c (ToCT Word64)
  ToT Text = 'T_c (ToCT Text)
  ToT Bool = 'T_c (ToCT Bool)
  ToT ByteString = 'T_c (ToCT ByteString)
  ToT Mutez = 'T_c (ToCT Mutez)
  ToT Address = 'T_c (ToCT Address)
  ToT KeyHash = 'T_c (ToCT KeyHash)
  ToT Timestamp = 'T_c (ToCT Timestamp)

  ToT () = 'T_unit
  ToT (a, b) = 'T_pair (ToT a) (ToT b)
  ToT [a] = 'T_list (ToT a)
  ToT (Maybe a) = 'T_option (ToT a)
  ToT (Either a b) = 'T_or (ToT a) (ToT b)
  ToT (Set k) = 'T_set (ToCT k)
  ToT (Map k v) = 'T_map (ToCT k) (ToT v)
  ToT PublicKey = 'T_key
  ToT Signature = 'T_signature
