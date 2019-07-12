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
  , HasUField
  , HasUStoreForAllIn
  ) where

import Data.Singletons (SingI)
import qualified Data.Kind as Kind
import GHC.Generics ((:*:), (:+:))
import qualified GHC.Generics as G
import Data.Vinyl.Derived (Label)
import GHC.TypeLits (KnownSymbol, Symbol)
import Type.Reflection ((:~:)(Refl))

import Lorentz.Base
import Lorentz.Errors
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
      [ [mt|UStore: failed to unpack |]
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
  :: forall store name s.
     (KeyAccessC store name, ValueAccessC store name)
  => Label name
  -> (forall s0 any. GetUStoreKey store name : s0 :-> any)
  -> GetUStoreKey store name
      : GetUStoreValue store name
      : UStore store
      : s
  :-> UStore store : s
ustoreInsertNew label doFail =
  duupX @3 # duupX @2 # ustoreMem label #
  if_ doFail (ustoreInsert label)

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
        [ [mt|UStore: no field |]
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
-- /big/ store, but needs to know only about some submap(s) of it.
--
-- It can use all UStore operations for a particular name, key and
-- value without knowing whole template.
type HasUStore name key value store =
   ( KeyAccessC store name, ValueAccessC store name
   , GetUStoreKey store name ~ key
   , GetUStoreValue store name ~ value
   )

-- | This constraint can be used if a function needs to work with
-- /big/ store, but needs to know only about some field of it.
type HasUField name ty store =
   ( FieldAccessC store name
   , GetUStoreField store name ~ ty
   )

-- | Write down all sensisble constraints which given @store@ satisfies
-- and apply them to @constrained@.
--
-- This store should have '|~>' and 'UStoreField' fields in its immediate fields,
-- no deep inspection is performed.
type HasUStoreForAllIn store constrained =
  (Generic store, GHasStoreForAllIn constrained (G.Rep store))

type family GHasStoreForAllIn (store :: Kind.Type) (x :: Kind.Type -> Kind.Type)
            :: Constraint where
  GHasStoreForAllIn store (G.D1 _ x) = GHasStoreForAllIn store x
  GHasStoreForAllIn store (x :+: y) =
    (GHasStoreForAllIn store x, GHasStoreForAllIn store y)
  GHasStoreForAllIn store (x :*: y) =
    (GHasStoreForAllIn store x, GHasStoreForAllIn store y)
  GHasStoreForAllIn store (G.C1 _ x) = GHasStoreForAllIn store x
  GHasStoreForAllIn store (G.S1 ('G.MetaSel ('Just name) _ _ _)
                            (G.Rec0 (key |~> value))) =
    HasUStore name key value store
  GHasStoreForAllIn store (G.S1 ('G.MetaSel ('Just name) _ _ _)
                            (G.Rec0 (UStoreField value))) =
    HasUField name value store
  GHasStoreForAllIn _ G.V1 = ()
  GHasStoreForAllIn _ G.U1 = ()

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

-- | When you want to express a constraint like
-- "given big store contains all elements present in given small concrete store",
-- you can use 'HasUStoreForAllIn'.
--
-- Here @store@ is a big store, and we expect it to contain 'MyStoreTemplate'
-- entirely.
_sample7
  :: HasUStoreForAllIn MyStoreTemplate store
  => UStore store : s :-> Bool : Maybe ByteString : s
_sample7 = ustoreGetField #flag # dip (push "x" # ustoreGet #bytes)

-- | '_sample7' with @store@ instantiated to 'MyStoreTemplateBig'.
_sample7' :: UStore MyStoreTemplateBig : s :-> Bool : Maybe ByteString : s
_sample7' = _sample7
