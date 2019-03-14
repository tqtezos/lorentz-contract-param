{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson instructions in untyped model.

module Michelson.Untyped.Instr
  ( InstrAbstract (..)
  , Instr
  , Op (..)
  , ExtU
  , InstrExtU

  -- * Contract's address
  , OriginationOperation (..)
  , mkContractAddress
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Data (Data(..))
import qualified Data.Kind as K
import Formatting.Buildable (Buildable)

import Michelson.Untyped.Annotation (FieldAnn, TypeAnn, VarAnn)
import Michelson.Untyped.Contract (Contract)
import Michelson.Untyped.Type (Comparable, Type)
import Michelson.Untyped.Value (Value)
import Tezos.Address (Address, mkContractAddressRaw)
import Tezos.Core (Mutez)
import Tezos.Crypto (KeyHash)

-------------------------------------
-- Flattened types after macroexpander
-------------------------------------
type InstrExtU = ExtU InstrAbstract Op
type Instr = InstrAbstract Op
newtype Op = Op {unOp :: Instr}
  deriving stock (Generic)

deriving instance Eq (ExtU InstrAbstract Op) => Eq Op
deriving instance Show (ExtU InstrAbstract Op) => Show Op
deriving instance Buildable Instr => Buildable Op

-------------------------------------
-- Abstract instruction
-------------------------------------

-- | ExtU is extension of InstrAbstract by Morley instructions
type family ExtU (instr :: K.Type -> K.Type) :: K.Type -> K.Type

-- | Michelson instruction with abstract parameter `op`.  This
-- parameter is necessary, because at different stages of our pipeline
-- it will be different. Initially it can contain macros and
-- non-flattened instructions, but then it contains only vanilla
-- Michelson instructions.
data InstrAbstract op
  = EXT               (ExtU InstrAbstract op)
  | DROP
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
  | CONS              VarAnn -- TODO add TypeNote param
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
  -- TODO check on alphanet whether we can pass TypeNote
  | EXEC              VarAnn
  | DIP               [op]
  | FAILWITH
  | CAST              VarAnn Type
  | RENAME            VarAnn
  | PACK              VarAnn
  | UNPACK            VarAnn Type
  | CONCAT            VarAnn
  | SLICE             VarAnn
  | ISNAT             VarAnn
  | ADD               VarAnn
  | SUB               VarAnn
  | MUL               VarAnn
  | EDIV              VarAnn
  | ABS               VarAnn
  -- TODO why no varnote for NEG
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
  | CONTRACT          VarAnn Type
  | TRANSFER_TOKENS   VarAnn
  | SET_DELEGATE      VarAnn
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
  deriving (Generic)

deriving instance (Eq op, Eq (ExtU InstrAbstract op)) => Eq (InstrAbstract op)
deriving instance (Show op, Show (ExtU InstrAbstract op)) => Show (InstrAbstract op)
deriving instance Functor (ExtU InstrAbstract) => Functor InstrAbstract
deriving instance (Data op, Data (ExtU InstrAbstract op)) => Data (InstrAbstract op)

-- deriving instance (Buildable op, Buildable (ExtU InstrAbstract op)) => Buildable (InstrAbstract op)
-- instance Buildable op => Buildable (InstrAbstract op) where
--   build = genericF

----------------------------------------------------------------------------
-- Contract's address computation
--
-- Note: it might be a bit weird place for this functionality, but it's the
-- lowest layer where all necessary Michelson types are defined. We may
-- reconsider it later.
----------------------------------------------------------------------------

-- | Data necessary to originate a contract.
data OriginationOperation = OriginationOperation
  { ooManager :: !KeyHash
  -- ^ Manager of the contract.
  , ooDelegate :: !(Maybe KeyHash)
  -- ^ Optional delegate.
  , ooSpendable :: !Bool
  -- ^ Whether the contract is spendable.
  , ooDelegatable :: !Bool
  -- ^ Whether the contract is delegatable.
  , ooBalance :: !Mutez
  -- ^ Initial balance of the contract.
  , ooStorage :: !(Value Op)
  -- ^ Initial storage value of the contract.
  , ooContract :: !(Contract Op)
  -- ^ The contract itself.
  } deriving (Generic)

deriving instance Show (ExtU InstrAbstract Op) => Show OriginationOperation

-- | Compute address of a contract from its origination operation.
--
-- TODO [TM-62] It's certainly imprecise, real Tezos implementation doesn't
-- use JSON, but we don't need precise format yet, so we just use some
-- serialization format (JSON because we have necessary instances already).
mkContractAddress :: Aeson.ToJSON InstrExtU => OriginationOperation -> Address
mkContractAddress = mkContractAddressRaw . BSL.toStrict . Aeson.encode

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

instance Aeson.ToJSON Instr => Aeson.ToJSON Op
instance Aeson.FromJSON Instr => Aeson.FromJSON Op
instance (Aeson.ToJSON op, Aeson.ToJSON (ExtU InstrAbstract op)) => Aeson.ToJSON (InstrAbstract op)
instance (Aeson.FromJSON op, Aeson.FromJSON (ExtU InstrAbstract op)) => Aeson.FromJSON (InstrAbstract op)
instance Aeson.FromJSON Op => Aeson.FromJSON OriginationOperation
instance Aeson.ToJSON Op => Aeson.ToJSON OriginationOperation
