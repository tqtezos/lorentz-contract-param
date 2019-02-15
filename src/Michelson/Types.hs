{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Michelson.Types
  (
  -- * Contract basics
    Parameter
  , Storage
  , Contract (..)

    -- * Data types
  , Timestamp (..)
  , Mutez (..)
  , Address (..)
  , Value (..)
  , Elt (..)
  , NetworkOp (..)
  , contractAddress

  -- Typechecker types
  , InstrAbstract (..)
  , Instr
  , Op (..)

    -- * Michelson types
  , Annotation (..)
  , pattern WithAnn
  , TypeAnn
  , FieldAnn
  , VarAnn
  , noAnn
  , ann
  , unifyAnn
  , ifAnnUnified
  , disjoinVn
  , convAnn
  , Type (..)
  , Comparable (..)
  , compToType
  , typeToComp
  , T (..)
  , CT (..)
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

import Data.Aeson
  (FromJSON(..), FromJSONKey, ToJSON(..), ToJSONKey, genericParseJSON, genericToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Default (Default(..))
import qualified Data.Text as T
import Formatting.Buildable (Buildable)

type Parameter = Type
type Storage = Type
data Contract op = Contract
  { para :: Parameter
  , stor :: Storage
  , code :: [op]
  } deriving (Eq, Show, Functor, Data, Generic)

-------------------------------------
-- Flattened types after macroexpander
-------------------------------------
type Instr = InstrAbstract Op
newtype Op = Op {unOp :: Instr}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-------------------------------------
-- Basic polymorphic types for Parser/Macro/Typechecker modules
-------------------------------------

newtype Timestamp = Timestamp
  { unTimestamp :: Word64
  } deriving stock (Show, Eq, Ord, Data)
    deriving newtype (ToJSON, FromJSON)

newtype Mutez = Mutez
  { unMutez :: Word64
  } deriving stock (Show, Eq, Ord, Data)
    deriving newtype (ToJSON, FromJSON)

newtype Address = Address
  { unAddress :: Text
  } deriving stock (Show, Eq, Ord, Data)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Buildable)

-- TODO [TM-17] I guess it's possible to compute address of a contract, but I
-- don't know how do it (yet). Maybe it requires more data. In the
-- worst case we can store such map in GState. Maybe we'll have to
-- move this function to Morley.
contractAddress :: Contract Op -> Address
contractAddress _ = Address "dummy-address"

{- Data types -}
data Value op =
    ValueInt     Integer
  | ValueString  Text
  | ValueBytes   ByteString
  | ValueUnit
  | ValueTrue
  | ValueFalse
  | ValuePair    (Value op) (Value op)
  | ValueLeft    (Value op)
  | ValueRight   (Value op)
  | ValueSome    (Value op)
  | ValueNone
  | ValueSeq     [Value op]
  -- ^ A sequence of elements: can be a list or a set.
  -- We can't distinguish lists and sets during parsing.
  | ValueMap     [Elt op]
  | ValueLambda  [op]
  deriving (Eq, Show, Functor, Data, Generic)

data Elt op = Elt (Value op) (Value op)
  deriving (Eq, Show, Functor, Data, Generic)

-- | Corresponds to the @operation@ type in Michelson.
-- TODO: add actual data.
data NetworkOp
  = CreateContract
  | CreateAccount
  | TransferTokens
  | SetDelegate

data InstrAbstract op =
    DROP
  | DUP               VarAnn
  | SWAP
  | PUSH              VarAnn Type (Value op)
  | SOME              TypeAnn VarAnn FieldAnn
  | NONE              TypeAnn VarAnn FieldAnn Type
  | UNIT              TypeAnn VarAnn
  | IF_NONE           [op] [op]
  | PAIR              TypeAnn VarAnn FieldAnn FieldAnn
  | CAR               VarAnn FieldAnn
  | CDR               VarAnn FieldAnn
  | LEFT              TypeAnn VarAnn FieldAnn FieldAnn Type
  | RIGHT             TypeAnn VarAnn FieldAnn FieldAnn Type
  | IF_LEFT           [op] [op]
  | IF_RIGHT          [op] [op]
  | NIL               TypeAnn VarAnn Type
  | CONS              VarAnn
  | IF_CONS           [op] [op]
  | SIZE              VarAnn
  | EMPTY_SET         TypeAnn VarAnn Comparable
  | EMPTY_MAP         TypeAnn VarAnn Comparable Type
  | MAP               VarAnn [op]
  | ITER              [op]
  | MEM               VarAnn
  | GET               VarAnn
  | UPDATE
  | IF                [op] [op]
  | LOOP              [op]
  | LOOP_LEFT         [op]
  | LAMBDA            VarAnn Type Type [op]
  | EXEC              VarAnn
  | DIP               [op]
  | FAILWITH
  | CAST              VarAnn Type
  | RENAME            VarAnn
  | PACK              VarAnn
  | UNPACK            VarAnn Type
  | CONCAT            VarAnn
  | SLICE             VarAnn
  | ISNAT
  | ADD               VarAnn
  | SUB               VarAnn
  | MUL               VarAnn
  | EDIV              VarAnn
  | ABS               VarAnn
  | NEG
  | LSL               VarAnn
  | LSR               VarAnn
  | OR                VarAnn
  | AND               VarAnn
  | XOR               VarAnn
  | NOT               VarAnn
  | COMPARE           VarAnn
  | EQ                VarAnn
  | NEQ               VarAnn
  | LT                VarAnn
  | GT                VarAnn
  | LE                VarAnn
  | GE                VarAnn
  | INT               VarAnn
  | SELF              VarAnn
  | CONTRACT          Type
  | TRANSFER_TOKENS   VarAnn
  | SET_DELEGATE
  | CREATE_ACCOUNT    VarAnn VarAnn
  | CREATE_CONTRACT   VarAnn VarAnn
  | CREATE_CONTRACT2  VarAnn VarAnn (Contract op)
  | IMPLICIT_ACCOUNT  VarAnn
  | NOW               VarAnn
  | AMOUNT            VarAnn
  | BALANCE           VarAnn
  | CHECK_SIGNATURE   VarAnn
  | SHA256            VarAnn
  | SHA512            VarAnn
  | BLAKE2B           VarAnn
  | HASH_KEY          VarAnn
  | STEPS_TO_QUOTA    VarAnn
  | SOURCE            VarAnn
  | SENDER            VarAnn
  | ADDRESS           VarAnn
  deriving (Eq, Show, Functor, Data, Generic)

-------------------------------------
-- Basic types for Michelson types --
-------------------------------------
newtype Annotation tag = Annotation T.Text
  deriving (Eq, Show, Data, Functor)
  deriving newtype (ToJSON, FromJSON)

instance Default (Annotation tag) where
  def = Annotation ""

data TypeTag
data FieldTag
data VarTag

type TypeAnn = Annotation TypeTag
type FieldAnn = Annotation FieldTag
type VarAnn = Annotation VarTag

noAnn :: Annotation a
noAnn = Annotation ""

ann :: T.Text -> Annotation a
ann = Annotation

instance Semigroup VarAnn where
  Annotation a <> Annotation b
    | a == "" || b == "" = ann $ a <> b
    | otherwise          = ann $ a <> "." <> b

instance Monoid VarAnn where
    mempty = noAnn

unifyAnn :: Annotation tag -> Annotation tag -> Maybe (Annotation tag)
unifyAnn (Annotation ann1) (Annotation ann2)
  | ann1 == "" || ann2 == "" = Just $ ann $ ann1 <> ann2
  | ann1 == ann2 = Just $ ann ann1
  | otherwise  = Nothing

ifAnnUnified :: Annotation tag -> Annotation tag -> Bool
ifAnnUnified a1 a2 = isJust $ a1 `unifyAnn` a2

disjoinVn :: VarAnn -> (VarAnn, VarAnn)
disjoinVn (Annotation a) = case T.findIndex (== '.') $ T.reverse a of
  Just ((n - 1 -) -> pos) -> (ann $ T.take pos a, ann $ T.drop (pos + 1) a)
  Nothing                 -> (noAnn, ann a)
  where
    n = T.length a

convAnn :: Annotation tag1 -> Annotation tag2
convAnn (Annotation a) = Annotation a

pattern WithAnn :: Annotation tag -> Annotation tag
pattern WithAnn ann <- ann@(Annotation (toString -> _:_))

-- Annotated type
data Type = Type T TypeAnn
  deriving (Eq, Show, Data)

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeAnn
  deriving (Eq, Show, Data)

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
  deriving (Eq, Show, Data)

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
  deriving (Eq, Show, Data)

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

----------------------------------------------------------------------------
-- JSON serialization
--
-- TODO:
-- 1. Get rid of dirty hack with bytestrings (unsuppress `-Worphans` once done).
-- 2. Figure out whether it's possible to use TH for types with parameters.
-- 3. Maybe write it manually using some specific format, e. g. use JSON
-- syntax from Michelson specification.
----------------------------------------------------------------------------

-- FIXME: this is a very bad dirty hack, it's temporary.
instance ToJSON ByteString where
  toJSON = toJSON @Text . decodeUtf8

-- FIXME: this is a very bad dirty hack, it's temporary.
instance FromJSON ByteString where
  parseJSON = fmap (encodeUtf8 @Text) . parseJSON

-- deriveJSON defaultOptions ''Contract
instance ToJSON op => ToJSON (Contract op) where
  toJSON = genericToJSON defaultOptions

instance FromJSON op => FromJSON (Contract op) where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON op => ToJSON (InstrAbstract op) where
  toJSON = genericToJSON defaultOptions

instance FromJSON op => FromJSON (InstrAbstract op) where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON op => ToJSON (Value op) where
  toJSON = genericToJSON defaultOptions

instance FromJSON op => FromJSON (Value op) where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON op => ToJSON (Elt op) where
  toJSON = genericToJSON defaultOptions

instance FromJSON op => FromJSON (Elt op) where
  parseJSON = genericParseJSON defaultOptions

deriveJSON defaultOptions ''Type
deriveJSON defaultOptions ''Comparable
deriveJSON defaultOptions ''T
deriveJSON defaultOptions ''CT

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
