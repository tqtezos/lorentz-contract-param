{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson types represented in untyped model.

module Michelson.Untyped.Type
  ( Type (..)
  , Comparable (..)
  , compToType
  , typeToComp
  , T (..)
  , CT (..)
  , ToCT
  , pattern Tint
  , pattern Tnat
  , pattern Tstring
  , pattern Tbytes
  , pattern Tmutez
  , pattern Tbool
  , pattern Tkey_hash
  , pattern Ttimestamp
  , pattern Taddress
  , tint
  , tnat
  , tstring
  , tbytes
  , tmutez
  , tbool
  , tkeyHash
  , ttimestamp
  , taddress
  , isAtomicType
  , isKey
  , isSignature
  , isComparable
  , isMutez
  , isKeyHash
  , isBool
  , isString
  , isInteger
  , isTimestamp
  , isNat
  , isInt
  , isBytes
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Text.Lazy.Builder (Builder)
import Fmt ((+|), (|+))
import Formatting.Buildable (Buildable(build))

import Michelson.Untyped.Annotation
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

-- Annotated type
data Type = Type T TypeAnn
  deriving (Eq, Show, Data, Generic)

instance Buildable Type where
  build (Type t a) = t |+ " " +| a |+ ""

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeAnn
  deriving (Eq, Show, Data, Generic)

instance Buildable Comparable where
  build (Comparable ct a)
    | a == noAnn = build ct
    | otherwise = ct |+ " " +| a |+ ""

compToType :: Comparable -> Type
compToType (Comparable ct tn) = Type (T_comparable ct) tn

typeToComp :: Type -> Maybe Comparable
typeToComp (Type (T_comparable ct) tn) = Just $ Comparable ct tn
typeToComp _ = Nothing

-- Michelson Type
data T =
    T_comparable CT
  | T_key
  | T_unit
  | T_signature
  | T_option FieldAnn Type
  | T_list Type
  | T_set Comparable
  | T_operation
  | T_contract Type
  | T_pair FieldAnn FieldAnn Type Type
  | T_or FieldAnn FieldAnn Type Type
  | T_lambda Type Type
  | T_map Comparable Type
  | T_big_map Comparable Type
  deriving (Eq, Show, Data, Generic)

instance Buildable T where
  build =
    \case
      T_comparable ct -> build ct
      T_key -> "key"
      T_unit -> "unit"
      T_signature -> "signature"
      T_option fa t -> "option (" +| t |+ " " +| fa |+ ")"
      T_list t -> "list (" +| t |+ ")"
      T_set c -> "set (" +| c |+ ")"
      T_operation -> "operation"
      T_contract t -> "contract " +| t |+ ""
      T_pair fa1 fa2 t1 t2 ->
        "pair (" +| t1 |+ " " +| fa1 |+ ")"
         +| " (" +| t2 |+ " " +| fa2 |+ ")"
      T_or fa1 fa2 t1 t2 ->
        "or ("   +| t1 |+ " " +| fa1 |+ ")"
         +| " (" +| t2 |+ " " +| fa2 |+ ")"
      T_lambda t1 t2 -> build2 "lambda" t1 t2
      T_map t1 t2 -> build2 "map" t1 t2
      T_big_map t1 t2 -> build2 "big_map" t1 t2
    where
      -- build something with 2 type parameters
      build2 :: (Buildable t1, Buildable t2) => Builder -> t1 -> t2 -> Builder
      build2 name t1 t2 = name |+ " (" +| t1 |+ " " +| t2 |+ ")"

-- Comparable Sub-Type
data CT =
    T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address
  deriving (Eq, Ord, Show, Data, Enum, Bounded, Generic)

-- | Type function that converts a regular Haskell type into a comparable type
-- (which has kind @CT@)
type family ToCT a :: CT where
  ToCT Integer = 'T_int
  ToCT Int = 'T_int
  ToCT Natural = 'T_nat
  ToCT Word64 = 'T_nat
  ToCT Text = 'T_string
  ToCT Bool = 'T_bool
  ToCT ByteString = 'T_bytes
  ToCT Mutez = 'T_mutez
  ToCT Address = 'T_address
  ToCT KeyHash = 'T_key_hash
  ToCT Timestamp = 'T_timestamp

instance Buildable CT where
  build =
    \case
      T_int -> "int"
      T_nat -> "nat"
      T_string -> "string"
      T_bytes -> "bytes"
      T_mutez -> "mutez"
      T_bool -> "bool"
      T_key_hash -> "key_hash"
      T_timestamp -> "timestamp"
      T_address -> "address"

pattern Tint :: T
pattern Tint <- T_comparable T_int

pattern Tnat :: T
pattern Tnat <- T_comparable T_nat

pattern Tstring :: T
pattern Tstring <- T_comparable T_string

pattern Tbytes :: T
pattern Tbytes <- T_comparable T_bytes

pattern Tmutez :: T
pattern Tmutez <- T_comparable T_mutez

pattern Tbool :: T
pattern Tbool <- T_comparable T_bool

pattern Tkey_hash :: T
pattern Tkey_hash <- T_comparable T_key_hash

pattern Ttimestamp :: T
pattern Ttimestamp <- T_comparable T_timestamp

pattern Taddress :: T
pattern Taddress <- T_comparable T_address

tint :: T
tint = T_comparable T_int

tnat :: T
tnat = T_comparable T_nat

tstring :: T
tstring = T_comparable T_string

tbytes :: T
tbytes = T_comparable T_bytes

tmutez :: T
tmutez = T_comparable T_mutez

tbool :: T
tbool = T_comparable T_bool

tkeyHash :: T
tkeyHash = T_comparable T_key_hash

ttimestamp :: T
ttimestamp = T_comparable T_timestamp

taddress :: T
taddress = T_comparable T_address

isAtomicType :: Type -> Bool
isAtomicType t@(Type _ (Annotation "")) =
    isComparable t || isKey t || isUnit t || isSignature t || isOperation t
isAtomicType _ = False

isKey :: Type -> Bool
isKey (Type T_key _) = True
isKey _              = False

isUnit :: Type -> Bool
isUnit (Type T_unit _) = True
isUnit _               = False

isSignature :: Type -> Bool
isSignature (Type T_signature _) = True
isSignature _                    = False

isOperation :: Type -> Bool
isOperation (Type T_operation _) = True
isOperation _                    = False

isComparable :: Type -> Bool
isComparable (Type (T_comparable _) _) = True
isComparable _ = False

isMutez :: Type -> Bool
isMutez (Type (T_comparable T_mutez) _) = True
isMutez _ = False

isTimestamp :: Type -> Bool
isTimestamp (Type (T_comparable T_timestamp) _) = True
isTimestamp _ = False

isKeyHash :: Type -> Bool
isKeyHash (Type (T_comparable T_key_hash) _) = True
isKeyHash _ = False

isBool  :: Type -> Bool
isBool (Type (T_comparable T_bool) _) = True
isBool _ = False

isString  :: Type -> Bool
isString (Type (T_comparable T_string) _) = True
isString _ = False

isInteger :: Type -> Bool
isInteger a = isNat a || isInt a || isMutez a || isTimestamp a

isNat  :: Type -> Bool
isNat (Type (T_comparable T_nat) _) = True
isNat _ = False

isInt  :: Type -> Bool
isInt (Type (T_comparable T_int) _) = True
isInt _ = False

isBytes :: Type -> Bool
isBytes (Type (T_comparable T_bytes) _) = True
isBytes _ = False

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

deriveJSON defaultOptions ''Type
deriveJSON defaultOptions ''Comparable
deriveJSON defaultOptions ''T
deriveJSON defaultOptions ''CT
