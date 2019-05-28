{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson instructions in untyped model.

module Michelson.Untyped.Instr
  ( InstrAbstract (..)
  , Op (..)
  , Instr
  , ExpandedOp (..)
  , ExpandedInstr
  , InstrExtU
  , ExpandedInstrExtU
  , flattenExpandedOp

  -- * Contract's address
  , OriginationOperation (..)
  , mkContractAddress
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Data (Data(..))
import Fmt (Buildable(build), (+|), (|+))
import Generics.SYB (everywhere, mkT)
import Prelude hiding (EQ, GT, LT)
import Text.PrettyPrint.Leijen.Text (braces, nest, (<$$>), (<+>))

import Michelson.ErrorPos (InstrCallStack)
import Michelson.Printer.Util (RenderDoc(..), buildRenderDoc, renderOpsList, spaces)
import Michelson.Untyped.Annotation
  (FieldAnn, TypeAnn, VarAnn, RenderAnn(..))
import Michelson.Untyped.Contract (Contract'(..))
import Michelson.Untyped.Ext (ExtInstrAbstract)
import Michelson.Untyped.Type (Comparable, Type)
import Michelson.Untyped.Value (Value'(..))
import Tezos.Address (Address, mkContractAddressRaw)
import Tezos.Core (Mutez)
import Tezos.Crypto (KeyHash)

-------------------------------------
-- Flattened types after macroexpander
-------------------------------------
type InstrExtU = ExtInstrAbstract Op
type Instr = InstrAbstract Op
newtype Op = Op {unOp :: Instr}
  deriving stock (Show, Eq, Generic)
  deriving newtype (RenderDoc, Buildable)

-------------------------------------
-- Types after macroexpander
-------------------------------------

type ExpandedInstrExtU = ExtInstrAbstract ExpandedOp
type ExpandedInstr = InstrAbstract ExpandedOp

data ExpandedOp
  = PrimEx ExpandedInstr
  | SeqEx [ExpandedOp]
  | WithSrcEx InstrCallStack ExpandedOp
  deriving stock (Show, Eq, Data, Generic)

instance RenderDoc ExpandedOp where
  renderDoc (WithSrcEx _ op) = renderDoc op
  renderDoc (PrimEx i) = renderDoc i
  renderDoc (SeqEx i)    = renderOpsList True i
  isRenderable =
    \case PrimEx i -> isRenderable i
          WithSrcEx _ op -> isRenderable op
          _ -> True

instance Buildable ExpandedOp where
  build (WithSrcEx _ op) = build op
  build (PrimEx expandedInstr) = "<PrimEx: "+|expandedInstr|+">"
  build (SeqEx expandedOps)    = "<SeqEx: "+|expandedOps|+">"

-- | Flatten all 'SeqEx' in 'ExpandedOp'. This function is mostly for
-- testing. It returns instructions with the same logic, but they are
-- not strictly equivalent, because they are serialized differently
-- (grouping instructions into sequences affects the way they are
-- PACK'ed).
--
-- Note: it does not return a list of 'Instr' because this type is not
-- used anywhere and should probably be removed.
flattenExpandedOp :: ExpandedOp -> [ExpandedInstr]
flattenExpandedOp =
  \case
    PrimEx i -> [flattenInstr i]
    SeqEx ops -> concatMap flattenExpandedOp ops
    WithSrcEx _ op -> flattenExpandedOp op
  where
    flattenInstr :: ExpandedInstr -> ExpandedInstr
    flattenInstr = everywhere (mkT flattenOps)

    flattenOps :: [ExpandedOp] -> [ExpandedOp]
    flattenOps [] = []
    flattenOps (SeqEx s : xs) = s ++ flattenOps xs
    flattenOps (x@(PrimEx _) : xs) = x : flattenOps xs
    flattenOps (WithSrcEx _ op : xs) = op : flattenOps xs

-------------------------------------
-- Abstract instruction
-------------------------------------

-- | Michelson instruction with abstract parameter `op`.  This
-- parameter is necessary, because at different stages of our pipeline
-- it will be different. Initially it can contain macros and
-- non-flattened instructions, but then it contains only vanilla
-- Michelson instructions.
data InstrAbstract op
  = EXT               (ExtInstrAbstract op)
  | DROP
  | DUP               VarAnn
  | SWAP
  | PUSH              VarAnn Type (Value' op)
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
  | CREATE_CONTRACT   VarAnn VarAnn (Contract' op)
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

instance (RenderDoc op) => RenderDoc (InstrAbstract op) where
  renderDoc = \case
    EXT extInstr          -> renderDoc extInstr
    DROP                  -> "DROP"
    DUP va                -> "DUP" <+> renderDoc va
    SWAP                  -> "SWAP"
    PUSH va t v           -> "PUSH" <+> renderDoc va <+> renderDoc t <+> renderDoc v
    SOME ta va fa         -> "SOME" <+> renderDoc ta <+> renderDoc va <+> renderDoc fa
    NONE ta va fa t       -> "NONE" <+> renderDoc ta <+> renderDoc va <+> renderDoc fa <+> renderDoc t
    UNIT ta va            -> "UNIT" <+> renderDoc ta <+> renderDoc va
    IF_NONE x y           -> "IF_NONE" <+> nest 9 (renderOps x) <$$> spaces 8 <> nest 9 (renderOps y)
    PAIR ta va fa1 fa2    -> "PAIR" <+> renderDoc ta <+> renderDoc va <+> renderAnn fa1 <+> renderAnn fa2
    CAR va fa             -> "CAR" <+> renderDoc va <+> renderDoc fa
    CDR va fa             -> "CDR" <+> renderDoc va <+> renderDoc fa
    LEFT ta va fa1 fa2 t  -> "LEFT" <+> renderDoc ta <+> renderDoc va <+> renderAnn fa1 <+> renderAnn fa2 <+> renderDoc t
    RIGHT ta va fa1 fa2 t -> "RIGHT" <+> renderDoc ta <+> renderDoc va <+> renderAnn fa1 <+> renderAnn fa2 <+> renderDoc t
    IF_LEFT x y           -> "IF_LEFT" <+> nest 9 (renderOps x) <$$> spaces 8 <> nest 9 (renderOps y)
    NIL ta va t           -> "NIL" <+> renderDoc ta <+> renderDoc va <+> renderDoc t
    CONS va               -> "CONS" <+> renderDoc va
    IF_CONS x y           -> "IF_CONS" <+> nest 9 (renderOps x) <$$> spaces 8 <> nest 9 (renderOps y)
    SIZE va               -> "SIZE" <+> renderDoc va
    EMPTY_SET ta va t     -> "EMPTY_SET" <+> renderDoc ta <+> renderDoc va <+> renderDoc t
    EMPTY_MAP ta va c t   -> "EMPTY_MAP" <+> renderDoc ta <+> renderDoc va <+> renderDoc c <+> renderDoc t
    MAP va s              -> "MAP" <+> renderDoc va <$$> spaces 4 <> nest 5 (renderOps s)
    ITER s                -> "ITER" <+> nest 6 (renderOps s)
    MEM va                -> "MEM" <+> renderDoc va
    GET va                -> "GET" <+> renderDoc va
    UPDATE                -> "UPDATE"
    IF x y                -> "IF" <+> nest 4 (renderOps x) <$$> spaces 3 <> nest 4 (renderOps y)
    LOOP s                -> "LOOP" <+> nest 6 (renderOps s)
    LOOP_LEFT s           -> "LOOP_LEFT" <+> nest 11 (renderOps s)
    LAMBDA va t r s       -> "LAMBDA" <+> renderDoc va <+> renderDoc t <+> renderDoc r <$$> spaces 7 <> nest 8 (renderOps s)
    EXEC va               -> "EXEC" <+> renderDoc va
    DIP s                 -> "DIP" <+> nest 5 (renderOps s)
    FAILWITH              -> "FAILWITH"
    CAST va t             -> "CAST" <+> renderDoc va <+> renderDoc t
    RENAME va             -> "RENAME" <+> renderDoc va
    PACK va               -> "PACK" <+> renderDoc va
    UNPACK va t           -> "UNPACK" <+> renderDoc va <+> renderDoc t
    CONCAT va             -> "CONCAT" <+> renderDoc va
    SLICE va              -> "SLICE" <+> renderDoc va
    ISNAT va              -> "ISNAT" <+> renderDoc va
    ADD va                -> "ADD" <+> renderDoc va
    SUB va                -> "SUB" <+> renderDoc va
    MUL va                -> "MUL" <+> renderDoc va
    EDIV va               -> "EDIV" <+> renderDoc va
    ABS va                -> "ABS" <+> renderDoc va
    NEG                   -> "NEG"
    LSL va                -> "LSL" <+> renderDoc va
    LSR va                -> "LSR" <+> renderDoc va
    OR  va                -> "OR" <+> renderDoc va
    AND va                -> "AND" <+> renderDoc va
    XOR va                -> "XOR" <+> renderDoc va
    NOT va                -> "NOT" <+> renderDoc va
    COMPARE va            -> "COMPARE" <+> renderDoc va
    EQ va                 -> "EQ" <+> renderDoc va
    NEQ va                -> "NEQ" <+> renderDoc va
    LT va                 -> "LT" <+> renderDoc va
    GT va                 -> "GT" <+> renderDoc va
    LE va                 -> "LE" <+> renderDoc va
    GE va                 -> "GE" <+> renderDoc va
    INT va                -> "INT" <+> renderDoc va
    SELF va               -> "SELF" <+> renderDoc va
    CONTRACT va t         -> "CONTRACT" <+> renderDoc va <+> renderDoc t
    TRANSFER_TOKENS va    -> "TRANSFER_TOKENS" <+> renderDoc va
    SET_DELEGATE va       -> "SET_DELEGATE" <+> renderDoc va
    CREATE_ACCOUNT va1 va2  -> "CREATE_ACCOUNT" <+> renderAnn va1 <+> renderAnn va2
    CREATE_CONTRACT va1 va2 contract ->
      "CREATE_CONTRACT" <+> renderAnn va1 <+> renderAnn va2 <$$> braces (renderDoc contract)
    IMPLICIT_ACCOUNT va   -> "IMPLICIT_ACCOUNT" <+> renderDoc va
    NOW va                -> "NOW" <+> renderDoc va
    AMOUNT va             -> "AMOUNT" <+> renderDoc va
    BALANCE va            -> "BALANCE" <+> renderDoc va
    CHECK_SIGNATURE va    -> "CHECK_SIGNATURE" <+> renderDoc va
    SHA256 va             -> "SHA256" <+> renderDoc va
    SHA512 va             -> "SHA512" <+> renderDoc va
    BLAKE2B va            -> "BLAKE2B" <+> renderDoc va
    HASH_KEY va           -> "HASH_KEY" <+> renderDoc va
    STEPS_TO_QUOTA va     -> "STEPS_TO_QUOTA" <+> renderDoc va
    SOURCE va             -> "SOURCE" <+> renderDoc va
    SENDER va             -> "SENDER" <+> renderDoc va
    ADDRESS va            -> "ADDRESS" <+> renderDoc va
    where
      renderOps = renderOpsList True

  isRenderable = \case
    EXT extInstr -> isRenderable extInstr
    _ -> True

instance (RenderDoc op, Buildable op) => Buildable (InstrAbstract op) where
  build = \case
    EXT ext -> build ext
    mi -> buildRenderDoc mi

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
  , ooStorage :: !(Value' ExpandedOp)
  -- ^ Initial storage value of the contract.
  , ooContract :: !(Contract' ExpandedOp)
  -- ^ The contract itself.
  } deriving (Show, Generic)

-- | Compute address of a contract from its origination operation.
--
-- TODO [TM-62] It's certainly imprecise, real Tezos implementation doesn't
-- use JSON, but we don't need precise format yet, so we just use some
-- serialization format (JSON because we have necessary instances already).
mkContractAddress :: OriginationOperation -> Address
mkContractAddress = mkContractAddressRaw . BSL.toStrict . Aeson.encode

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

instance Aeson.ToJSON Op
instance Aeson.FromJSON Op
instance Aeson.ToJSON ExpandedOp
instance Aeson.FromJSON ExpandedOp
instance Aeson.ToJSON op => Aeson.ToJSON (InstrAbstract op)
instance Aeson.FromJSON op => Aeson.FromJSON (InstrAbstract op)
instance Aeson.FromJSON OriginationOperation
instance Aeson.ToJSON OriginationOperation
