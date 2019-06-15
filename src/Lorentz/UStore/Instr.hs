{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Instructions to work with 'UStore'.
module Lorentz.UStore.Instr
  ( ustoreMem
  , ustoreGet
  , ustoreUpdate
  , ustoreInsert
  , ustoreInsertNew
  , ustoreDelete

  , ustoreToField
  , ustoreGetField
  , ustoreSetField

    -- ** Instruction constraints
  , HasUStore
  ) where

import Data.Singletons (SingI)
import Data.Vinyl.Derived (Label)
import GHC.TypeLits (KnownSymbol, Symbol)
import Type.Reflection ((:~:)(Refl))

import Lorentz.Base
import Lorentz.Errors
import Lorentz.Constraints
import Lorentz.Instr as L
import Lorentz.Macro
import Lorentz.UStore.Types
import Lorentz.UStore.Common
import Michelson.Text
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope

-- Helpers
----------------------------------------------------------------------------

type KeyAccessC store name =
  ( Each [Typeable, SingI, HasNoOp, HasNoBigMap] '[ToT (MSKey (GetUStore name store))]
  , KnownSymbol name
  )

type ValueAccessC store name =
  ( Each [Typeable, SingI, HasNoOp, HasNoBigMap] '[ToT (MSValue (GetUStore name store))]
  , KnownSymbol name
  )

type FieldAccessC store name =
  ( Each [Typeable, SingI, HasNoOp, HasNoBigMap] '[ToT (FSValue (GetUStore name store))]
  , KnownSymbol name
  )


packSubMapUKey
  :: forall (field :: Symbol) k s.
     (KnownSymbol field, Each [Typeable, SingI, HasNoOp, HasNoBigMap] '[ToT k])
  => (k : s) :-> (ByteString : s)
packSubMapUKey = push fieldName # pair # pack
  where
    fieldName = fieldNameToMText @field

pushFieldUKey
  :: forall (field :: Symbol) s.
     (KnownSymbol field)
  => s :-> (ByteString : s)
pushFieldUKey = push fieldName # pack
  where
    fieldName = fieldNameToMText @field

unpackUValueUnsafe
  :: forall (field :: Symbol) val s.
     ( KnownSymbol field
     , Each [Typeable, SingI, HasNoOp, HasNoBigMap] '[ToT val]
     )
  => (ByteString : s) :-> (val : s)
unpackUValueUnsafe = unpack @val # ifSome nop (failUsing failErr)
  where
    failErr = mconcat
      [ [mt|Failed to unpack UStore value under field: |]
      , fieldNameToMText @field
      ]

-- Main instructions
----------------------------------------------------------------------------

{- Note on store initialization:

It's not possible to create an empty store, because Michelson provides no way
to create a new empty @big_map@ from within the contract code.
-}

ustoreMem
  :: forall store name s.
     (KeyAccessC store name)
  => Label name
  -> GetUStoreKey store name : UStore store : s :-> Bool : s
ustoreMem _ = packSubMapUKey @name # mem

ustoreGet
  :: forall store name s.
     (KeyAccessC store name, ValueAccessC store name)
  => Label name
  -> GetUStoreKey store name : UStore store : s
       :-> Maybe (GetUStoreValue store name) : s
ustoreGet _ =
  packSubMapUKey @name #
  L.get #
  lmap (unpackUValueUnsafe @name @(GetUStoreValue store name))

ustoreUpdate
  :: forall store name s.
     (KeyAccessC store name, ValueAccessC store name)
  => Label name
  -> GetUStoreKey store name
      : Maybe (GetUStoreValue store name)
      : UStore store
      : s
  :-> UStore store : s
ustoreUpdate _ =
  packSubMapUKey @name #
  dip (lmap pack) #
  update

ustoreInsert
  :: forall store name s.
     (KeyAccessC store name, ValueAccessC store name)
  => Label name
  -> GetUStoreKey store name
      : GetUStoreValue store name
      : UStore store
      : s
  :-> UStore store : s
ustoreInsert _ =
  packSubMapUKey @name #
  dip (pack # L.some) #
  update

-- | Insert a key-value pair, but fail if it will overwrite some existing entry.
ustoreInsertNew
  :: forall store name err s.
     (KeyAccessC store name, ValueAccessC store name, KnownValue err)
  => Label name
  -> (forall s0. GetUStoreKey store name : s0 :-> err : s0)
  -> GetUStoreKey store name
      : GetUStoreValue store name
      : UStore store
      : s
  :-> UStore store : s
ustoreInsertNew label mkErr =
  duupX @3 # duupX @2 # ustoreMem label #
  if_ (mkErr # failWith)
      (ustoreInsert label)

ustoreDelete
  :: forall store name s.
     (KeyAccessC store name)
  => Label name
  -> GetUStoreKey store name : UStore store : s
     :-> UStore store : s
ustoreDelete _ =
  packSubMapUKey @name #
  dip none #
  update

-- | Like 'toField', but for 'UStore'.
--
-- This may fail only if 'UStore' was made up incorrectly during contract
-- initialization.
ustoreToField
  :: forall store name s.
     (FieldAccessC store name)
  => Label name
  -> UStore store : s
     :-> GetUStoreField store name : s
ustoreToField _ =
  pushFieldUKey @name #
  L.get #
  ensureFieldIsPresent #
  unpackUValueUnsafe @name @(GetUStoreField store name)
  where
    ensureFieldIsPresent =
      ifSome nop $ failUsing $ mconcat
        [ [mt|Plain field was no present in UStore, under field :|]
        , fieldNameToMText @name
        ]

-- | Like 'getField', but for 'UStore'.
--
-- This may fail only if 'UStore' was made up incorrectly during contract
-- initialization.
ustoreGetField
  :: forall store name s.
     (FieldAccessC store name)
  => Label name
  -> UStore store : s
     :-> GetUStoreField store name : UStore store : s
ustoreGetField label = dup # ustoreToField label

-- | Like 'setField', but for 'UStore'.
ustoreSetField
  :: forall store name s.
     (FieldAccessC store name)
  => Label name
  -> GetUStoreField store name : UStore store : s
     :-> UStore store : s
ustoreSetField _ =
  pack # L.some #
  pushFieldUKey @name #
  L.update

-- | This constraint can be used if a function needs to work with
-- /big/ store, but needs to know only about some part(s) of it.
--
-- It can use all UStore operations for a particular name, key and
-- value without knowing whole template.
type HasUStore name key value store =
   ( KeyAccessC store name, ValueAccessC store name
   , GetUStoreKey store name ~ key
   , GetUStoreValue store name ~ value
   )

----------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------

data MyStoreTemplate = MyStoreTemplate
  { ints :: Integer |~> ()
  , bytes :: ByteString |~> ByteString
  , flag :: UStoreField Bool
  }
  deriving stock (Generic)

type MyStore = UStore MyStoreTemplate

_sample1 :: Integer : MyStore : s :-> MyStore : s
_sample1 = ustoreDelete @MyStoreTemplate #ints

_sample2 :: ByteString : ByteString : MyStore : s :-> MyStore : s
_sample2 = ustoreInsert @MyStoreTemplate #bytes

_sample3 :: MyStore : s :-> Bool : s
_sample3 = ustoreToField @MyStoreTemplate #flag

data MyStoreTemplate2 = MyStoreTemplate2
  { bools :: Bool |~> Bool
  , ints2 :: Integer |~> Integer
  , ints3 :: Integer |~> Bool
  }
  deriving stock (Generic)

newtype MyNatural = MyNatural Natural
  deriving newtype (IsoCValue, IsoValue)

data MyStoreTemplate3 = MyStoreTemplate3 { store3 :: Natural |~> MyNatural }
  deriving stock Generic

data MyStoreTemplateBig = MyStoreTemplateBig
  MyStoreTemplate
  MyStoreTemplate2
  MyStoreTemplate3
  deriving stock Generic

_MyStoreTemplateBigTextsStore ::
  GetUStore "bytes" MyStoreTemplateBig :~: 'MapSignature ByteString ByteString
_MyStoreTemplateBigTextsStore = Refl

_MyStoreTemplateBigBoolsStore ::
  GetUStore "bools" MyStoreTemplateBig :~: 'MapSignature Bool Bool
_MyStoreTemplateBigBoolsStore = Refl

_MyStoreTemplateBigMyStoreTemplate3 ::
  GetUStore "store3" MyStoreTemplateBig :~: 'MapSignature Natural MyNatural
_MyStoreTemplateBigMyStoreTemplate3 = Refl

type MyStoreBig = UStore MyStoreTemplateBig

_sample4 :: Integer : MyStoreBig : s :-> MyStoreBig : s
_sample4 = ustoreDelete #ints2

_sample5 :: ByteString : MyStoreBig : s :-> Bool : s
_sample5 = ustoreMem #bytes

_sample6 :: Natural : MyNatural : MyStoreBig : s :-> MyStoreBig : s
_sample6 = ustoreInsert #store3
