{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Impementation of @Store@ - object incapsulating multiple 'BigMap's.
--
-- This module also provides template for the contract storage -
-- 'StorageSkeleton'.
--
-- We represent 'Store' as @big_map bytes (a | b | ...)@.
--
-- Key of this map is formed as @(index, orig_key)@, where @index@ is
-- zero-based index of emulated map, @orig_key@ is key of this emulated map.
--
-- Value of this map is just a union of emulated map's values.

{- Note on store inner representation (@martoon)

I see an alternative approach - representing store as
@big_map bytes (option a, option b, ...)@.

This would allow for saner implementation and more convenient interface.
An obvious shortcoming here is gas consumption. But this overhead seems
insignificant against the background of some other instructions.

-}
module Lorentz.Store
  ( -- * Store and related type definitions
    Store (..)
  , type (|->)

    -- ** Type-lookup-by-name
  , GetStoreKey
  , GetStoreValue

    -- ** Instructions
  , storeMem
  , storeGet
  , storeInsert
  , storeInsertNew
  , storeDelete

    -- ** Instruction constraints
  , StoreMemC
  , StoreGetC
  , StoreInsertC
  , StoreDeleteC

    -- * Storage skeleton
  , StorageSkeleton (..)
  , storageMem
  , storageGet
  , storageInsert
  , storageInsertNew
  , storageDelete

    -- * Store management from Haskell
  , storePiece
  , storeLookup
  ) where

import Data.Default (Default)
import qualified Data.Kind as Kind
import qualified Data.Map as Map
import Data.Singletons (SingI)
import Data.Type.Bool (If, type (||))
import Data.Type.Equality (type (==))
import Data.Vinyl.Derived (Label)
import GHC.Generics ((:+:))
import qualified GHC.Generics as G
import GHC.TypeLits (AppendSymbol, ErrorMessage(..), KnownSymbol, Symbol, TypeError)
import GHC.TypeNats (type (+))
import Type.Reflection ((:~:)(Refl))

import Lorentz.ADT
import Lorentz.Base
import Lorentz.Coercions
import Lorentz.Constraints
import Lorentz.Instr
import Lorentz.Macro
import Michelson.Interpret.Pack
import Michelson.Typed.Haskell.Instr.Sum
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Instr
import Michelson.Typed.Scope

{-# ANN module ("HLint: ignore Use 'natVal' from Universum" :: Text) #-}

----------------------------------------------------------------------------
-- Store
----------------------------------------------------------------------------

-- | Gathers multple 'BigMap's under one object.
--
-- Type argument of this datatype stands for a "map template" -
-- a datatype with multiple constructors, each containing an object of
-- type '|->' and corresponding to single virtual 'BigMap'.
-- It's also possible to parameterize it with a larger type which is
-- a sum of types satisfying the above property.
--
-- Inside it keeps only one 'BigMap' thus not violating Michelson limitations.
--
-- See examples below.
newtype Store a = Store { unStore :: BigMap ByteString a }
  deriving stock (Eq, Show)
  deriving newtype (Default, Semigroup, Monoid, IsoValue)

-- | Describes one virtual big map.
data k |-> v = BigMapImage v
  deriving stock Generic
  deriving anyclass IsoValue

{- Again we use generic magic to implement methods for 'Store'
(and thus 'Store' type constructor accepts a datatype, not a type-level list).

There are two reasons for this:

1. This gives us expected balanced tree of 'Or's for free.

2. This allows us selecting a map by constructor name, not by
e.g. type of map value. This is subjective, but looks like a good thing
for me (@martoon). On the other hand, it prevents us from sharing the
same interface between maps and 'Store'.

-}

-- | Position of a constructor in the corresponding datatype declaration.
type CtorIdx = Nat

-- | Number of datatype constructors.
type CtorsNum = Nat

-- | Type arguments of '|->'.
data MapSignature = MapSignature Kind.Type Kind.Type CtorIdx

-- Again, we will use these getters instead of binding types within
-- 'MapSignature' using type equality because getters does not produce extra
-- compile errors on "field not found" cases.
type family MSKey ms where
  MSKey ('MapSignature k _ _) = k
type family MSValue ms where
  MSValue ('MapSignature _ v _) = v
type family MSCtorIdx ms where
  MSCtorIdx ('MapSignature _ _ ci) = ci

-- | Get map signature from the constructor with a given name.
type GetStore name a = MSRequireFound name a (GLookupStore name (G.Rep a))

data MapLookupRes
  = MapFound MapSignature
  | MapAbsent CtorsNum

type family MSRequireFound
  (name :: Symbol)
  (a :: Kind.Type)
  (mlr :: MapLookupRes)
    :: MapSignature where
  MSRequireFound _ _ ('MapFound ms) = ms
  MSRequireFound name a ('MapAbsent _) = TypeError
    ('Text "Failed to find store template: datatype " ':<>: 'ShowType a ':<>:
     'Text " has no constructor " ':<>: 'ShowType name)

type family GLookupStore (name :: Symbol) (x :: Kind.Type -> Kind.Type)
              :: MapLookupRes where
  GLookupStore name (G.D1 _ x) = GLookupStore name x
  GLookupStore name (x :+: y) = LSMergeFound name (GLookupStore name x)
                                                  (GLookupStore name y)
  -- When we encounter a constructor there are two cases we are interested in:
  -- 1. This constructor has one field with type `|->`. Then we check its name
  -- and return 'MapFound' if it matches and 'MapAbsent' otherwise (storing
  -- information that we've found one constructor).
  -- 2. This constructor has one field with a different type. Then we expect
  -- this field to store '|->' somewhere deeper and try to find it there.
  GLookupStore name (G.C1 ('G.MetaCons ctorName _ _) x) =
    If (IsLeafCtor x)
      (If (name == ctorName || name == ("c" `AppendSymbol` ctorName))
         ('MapFound $ GExtractMapSignature ctorName x)
         ('MapAbsent 1)
      )
      (GLookupStoreDeeper name x)
  GLookupStore _ G.V1 = 'MapAbsent 0

-- Helper type family to check whether ADT constructor has one field
-- with type `|->`.
type family IsLeafCtor (x :: Kind.Type -> Kind.Type) :: Bool where
  IsLeafCtor (G.S1 _ (G.Rec0 (_ |-> _))) = 'True
  IsLeafCtor _ = 'False

-- Helper type family to go deeper during type-level store lookup.
type family GLookupStoreDeeper (name :: Symbol) (x :: Kind.Type -> Kind.Type)
              :: MapLookupRes where
  GLookupStoreDeeper name (G.S1 _ (G.Rec0 y))  = GLookupStore name (G.Rep y)
  GLookupStoreDeeper name _ = TypeError
    ('Text "Attempt to go deeper failed while looking for" ':<>: 'ShowType name
    ':$$:
    'Text "Make sure that all constructors have exactly one field inside.")

type family LSMergeFound (name :: Symbol)
  (f1 :: MapLookupRes) (f2 :: MapLookupRes)
  :: MapLookupRes where
  LSMergeFound _ ('MapAbsent n1) ('MapAbsent n2) = 'MapAbsent (n1 + n2)
  LSMergeFound _ ('MapFound ms) ('MapAbsent _) = 'MapFound ms
  LSMergeFound _ ('MapAbsent n) ('MapFound ('MapSignature k v i)) =
    'MapFound ('MapSignature k v (n + i))
  -- It's possible that there are two constructors with the same name,
  -- because main template pattern may be a sum of smaller template
  -- patterns with same constructor names.
  LSMergeFound ctor ('MapFound _) ('MapFound _) = TypeError
    ('Text "Found more than one constructor matching " ':<>: 'ShowType ctor)

type family GExtractMapSignature (ctor :: Symbol) (x :: Kind.Type -> Kind.Type)
             :: MapSignature where
  GExtractMapSignature _ (G.S1 _ (G.Rec0 (k |-> v))) = 'MapSignature k v 0
  GExtractMapSignature ctor _ = TypeError
    ('Text "Expected exactly one field of type `k |-> v`" ':$$:
     'Text "In constructor " ':<>: 'ShowType ctor)

type GetStoreKey store name = MSKey (GetStore name store)
type GetStoreValue store name = MSValue (GetStore name store)

packKey
  :: forall (idx :: CtorIdx) t s.
     (KnownNat idx, Typeable t, SingI t, HasNoOp t, HasNoBigMap t)
  => Instr (t : s) (ToT ByteString : s)
packKey =
  PUSH (toVal . natVal $ Proxy @idx) `Seq`
  PAIR @(ToT Natural) @t `Seq`
  PACK

type StoreOpC store name =
  ( Each [Typeable, SingI, HasNoOp, HasNoBigMap] '[ToT (MSKey (GetStore name store))]
  , KnownNat (MSCtorIdx (GetStore name store))
  )

{- Note on store initialization:

It's not possible to create an empty store, because Michelson provides no way
to create a new empty @big_map@.
-}

storeMem
  :: forall store name s.
     (StoreMemC store name)
  => Label name
  -> GetStoreKey store name : Store store : s :-> Bool : s
storeMem _ = I $
  packKey @(MSCtorIdx (GetStore name store)) `Seq`
  MEM

type StoreMemC store name = StoreOpC store name

storeGet
  :: forall store name s.
     StoreGetC store name
  => Label name
  -> GetStoreKey store name : Store store : s
       :-> Maybe (GetStoreValue store name) : s
storeGet label = I $
  packKey @(MSCtorIdx (GetStore name store)) `Seq`
  GET `Seq`
  IF_NONE (NONE) (instrUnwrapUnsafe @store label `Seq` SOME)

type StoreGetC store name =
  ( StoreOpC store name
  , InstrUnwrapC store name
  , SingI (ToT (GetStoreValue store name))
  , CtorHasOnlyField name store
      (GetStoreKey store name |-> GetStoreValue store name)
  )

storeInsert
  :: forall store name s.
     StoreInsertC store name
  => Label name
  -> GetStoreKey store name
      : GetStoreValue store name
      : Store store
      : s
  :-> Store store : s
storeInsert label = I $
  packKey @(MSCtorIdx (GetStore name store)) `Seq`
  DIP (instrWrap @store label `Seq` SOME) `Seq`
  UPDATE

type StoreInsertC store name =
  ( StoreOpC store name
  , InstrWrapC store name
  , CtorHasOnlyField name store
      (GetStoreKey store name |-> GetStoreValue store name)
  )

-- | Insert a key-value pair, but fail if it will overwrite some existing entry.
storeInsertNew
  :: forall store name err s.
     (StoreInsertC store name, KnownSymbol name, KnownValue err)
  => Label name
  -> (forall s0. GetStoreKey store name : s0 :-> err : s0)
  -> GetStoreKey store name
      : GetStoreValue store name
      : Store store
      : s
  :-> Store store : s
storeInsertNew label mkErr =
  duupX @3 # duupX @2 # storeMem label #
  if_ (mkErr # failWith)
      (storeInsert label)

storeDelete
  :: forall store name s.
     ( StoreDeleteC store name
     )
  => Label name
  -> GetStoreKey store name : Store store : s
     :-> Store store : s
storeDelete _ = I $
  packKey @(MSCtorIdx (GetStore name store)) `Seq`
  DIP (NONE @(ToT store)) `Seq`
  UPDATE

type StoreDeleteC store name =
  ( StoreOpC store name
  , SingI (ToT store)
  )

-- Examples
----------------------------------------------------------------------------

data MyStoreTemplate
  = IntsStore (Integer |-> ())
  | TextsStore (Text |-> ByteString)
  deriving stock Generic
  deriving anyclass IsoValue

type MyStore = Store MyStoreTemplate

_sample1 :: Integer : MyStore : s :-> MyStore : s
_sample1 = storeDelete @MyStoreTemplate #cIntsStore

_sample2 :: Text : ByteString : MyStore : s :-> MyStore : s
_sample2 = storeInsert @MyStoreTemplate #cTextsStore

data MyStoreTemplate2
  = BoolsStore (Bool |-> Bool)
  | IntsStore2 (Integer |-> Integer)
  | IntsStore3 (Integer |-> Bool)
  deriving stock Generic
  deriving anyclass IsoValue

-- You must derive 'Generic' instance for all custom types, even
-- newtypes.
newtype MyNatural = MyNatural Natural
  deriving stock Generic
  deriving newtype (IsoCValue, IsoValue)

data MyStoreTemplate3 = MyStoreTemplate3 (Natural |-> MyNatural)
  deriving stock Generic
  deriving anyclass IsoValue

data MyStoreTemplateBig
  = BigTemplatePart1 MyStoreTemplate
  | BigTemplatePart2 MyStoreTemplate2
  | BigTemplatePart3 MyStoreTemplate3
  deriving stock Generic
  deriving anyclass IsoValue

_MyStoreTemplateBigTextsStore ::
  GetStore "cTextsStore" MyStoreTemplateBig :~: 'MapSignature Text ByteString 1
_MyStoreTemplateBigTextsStore = Refl

_MyStoreTemplateBigBoolsStore ::
  GetStore "cBoolsStore" MyStoreTemplateBig :~: 'MapSignature Bool Bool 2
_MyStoreTemplateBigBoolsStore = Refl

_MyStoreTemplateBigMyStoreTemplate3 ::
  GetStore "cMyStoreTemplate3" MyStoreTemplateBig :~: 'MapSignature Natural MyNatural 5
_MyStoreTemplateBigMyStoreTemplate3 = Refl

type MyStoreBig = Store MyStoreTemplateBig

_sample3 :: Integer : MyStoreBig : s :-> MyStoreBig : s
_sample3 = storeDelete @MyStoreTemplateBig #cIntsStore2

_sample4 :: Text : MyStoreBig : s :-> Bool : s
_sample4 = storeMem @MyStoreTemplateBig #cTextsStore

_sample5 :: Natural : MyNatural : MyStoreBig : s :-> MyStoreBig : s
_sample5 = storeInsert @MyStoreTemplateBig #cMyStoreTemplate3

----------------------------------------------------------------------------
-- Storage skeleton
----------------------------------------------------------------------------

-- | Contract storage with @big_map@.
--
-- Due to Michelson constraints it is the only possible layout containing
-- @big_map@.
data StorageSkeleton storeTemplate other = StorageSkeleton
  { sMap :: Store storeTemplate
  , sFields :: other
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Default, IsoValue)

storageUnpack :: StorageSkeleton store fields : s :-> (Store store, fields) : s
storageUnpack = coerce_

storagePack :: (Store store, fields) : s :-> StorageSkeleton store fields : s
storagePack = coerce_

storageMem
  :: forall store name fields s.
     (StoreMemC store name)
  => Label name
  -> GetStoreKey store name : StorageSkeleton store fields : s :-> Bool : s
storageMem label = dip (storageUnpack # car) # storeMem label

storageGet
  :: forall store name fields s.
     StoreGetC store name
  => Label name
  -> GetStoreKey store name : StorageSkeleton store fields : s
       :-> Maybe (GetStoreValue store name) : s
storageGet label = dip (storageUnpack # car) # storeGet label

storageInsert
  :: forall store name fields s.
     StoreInsertC store name
  => Label name
  -> GetStoreKey store name
      : GetStoreValue store name
      : StorageSkeleton store fields
      : s
  :-> StorageSkeleton store fields : s
storageInsert label =
  dip (dip (storageUnpack # dup # car # dip cdr)) #
  storeInsert label #
  pair # storagePack

-- | Insert a key-value pair, but fail if it will overwrite some existing entry.
storageInsertNew
  :: forall store name fields err s.
     (StoreInsertC store name, KnownSymbol name, KnownValue err)
  => Label name
  -> (forall s0. GetStoreKey store name : s0 :-> err : s0)
  -> GetStoreKey store name
      : GetStoreValue store name
      : StorageSkeleton store fields
      : s
  :-> StorageSkeleton store fields : s
storageInsertNew label mkErr =
  dip (dip (storageUnpack # dup # car # dip cdr)) #
  storeInsertNew label mkErr #
  pair # storagePack

storageDelete
  :: forall store name fields s.
     ( StoreDeleteC store name
     )
  => Label name
  -> GetStoreKey store name : StorageSkeleton store fields : s
     :-> StorageSkeleton store fields : s
storageDelete label =
  dip (storageUnpack # dup # car # dip cdr) #
  storeDelete label #
  pair # storagePack

-- Examples
----------------------------------------------------------------------------

type MyStorage = StorageSkeleton MyStoreTemplate (Text, ByteString)

-- You can access both Store...
_storageSample1 :: Integer : MyStorage : s :-> MyStorage : s
_storageSample1 = storageDelete @MyStoreTemplate #cIntsStore

-- and other fields of the storage created with 'StorageSkeleton'.
_storageSample2 :: MyStorage : s :-> Text : s
_storageSample2 = toField #sFields # car

----------------------------------------------------------------------------
-- Store construction from Haskell
----------------------------------------------------------------------------

packHsKey
  :: forall ctorIdx key.
     ( IsoValue key, KnownValue key, HasNoOp (ToT key), HasNoBigMap (ToT key)
     , KnownNat ctorIdx
     )
  => key -> ByteString
packHsKey key =
  packValue' $ toVal (natVal (Proxy @ctorIdx), key)

-- | Lift a key-value pair to 'Store'.
--
-- Further you can use 'Monoid' instance of @Store@ to make up large stores.
storePiece
  :: forall name store key value ctorIdx.
     ( key ~ GetStoreKey store name
     , value ~ GetStoreValue store name
     , ctorIdx ~ MSCtorIdx (GetStore name store)
     , IsoValue key, KnownValue key, HasNoOp (ToT key), HasNoBigMap (ToT key)
     , KnownNat ctorIdx
     , InstrWrapC store name, Generic store
     , ExtractCtorField (GetCtorField store name) ~ (key |-> value)
     )
  => Label name
  -> key
  -> value
  -> Store store
storePiece label key val =
  Store . BigMap $ one
    ( packHsKey @ctorIdx key
    , hsWrap @store label (BigMapImage val)
    )

-- | Get a value from store by key.
--
-- It expects map to be consistent, otherwise call to this function fails
-- with error.
storeLookup
  :: forall name store key value ctorIdx.
     ( key ~ GetStoreKey store name
     , value ~ GetStoreValue store name
     , ctorIdx ~ MSCtorIdx (GetStore name store)
     , IsoValue key, KnownValue key, HasNoOp (ToT key), HasNoBigMap (ToT key)
     , KnownNat ctorIdx
     , InstrUnwrapC store name, Generic store
     , CtorOnlyField name store ~ (key |-> value)
     )
  => Label name
  -> key
  -> Store store
  -> Maybe value
storeLookup label key (Store (BigMap m)) =
  Map.lookup (packHsKey @ctorIdx key) m <&> \val ->
    case hsUnwrap label val of
      Nothing -> error "Invalid store, keys and values types \
                       \correspondence is violated"
      Just (BigMapImage x) -> x

-- Examples
----------------------------------------------------------------------------

_storeSample :: Store MyStoreTemplate
_storeSample = mconcat
  [ storePiece #cIntsStore 1 ()
  , storePiece #cTextsStore "a" "b"
  ]

_lookupSample :: Maybe ByteString
_lookupSample = storeLookup #cTextsStore "a" _storeSample

_storeSampleBig :: Store MyStoreTemplateBig
_storeSampleBig = mconcat
  [ storePiece #cIntsStore 1 ()
  , storePiece #cBoolsStore True True
  , storePiece #cIntsStore3 2 False
  ]

_lookupSampleBig :: Maybe Bool
_lookupSampleBig = storeLookup #cIntsStore3 2 _storeSampleBig
