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

import Lorentz.ADT
import Lorentz.Base
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
--
-- Inside it keeps only one 'BigMap' thusnot violating Michelson limitations.
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
    ('Text "Datatype " ':<>: 'ShowType a ':<>:
     'Text " has no constructor " ':<>: 'ShowType name)

type family GLookupStore (name :: Symbol) (x :: Kind.Type -> Kind.Type)
              :: MapLookupRes where
  GLookupStore name (G.D1 _ x) = GLookupStore name x
  GLookupStore name (x :+: y) = LSMergeFound (GLookupStore name x)
                                             (GLookupStore name y)
  GLookupStore name (G.C1 ('G.MetaCons ctorName _ _) x) =
    If (name == ctorName || name == ("c" `AppendSymbol` ctorName))
       ('MapFound $ GExtractMapSignature ctorName x)
       ('MapAbsent 1)
  GLookupStore _ G.V1 = 'MapAbsent 0

type family LSMergeFound (f1 :: MapLookupRes) (f2 :: MapLookupRes)
              :: MapLookupRes where
  LSMergeFound ('MapAbsent n1) ('MapAbsent n2) = 'MapAbsent (n1 + n2)
  LSMergeFound ('MapFound ms) ('MapAbsent _) = 'MapFound ms
  LSMergeFound ('MapAbsent n) ('MapFound ('MapSignature k v i)) =
    'MapFound ('MapSignature k v (n + i))
  -- the last case is not possible, constructor names are unique

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
     ( StoreOpC store name
     , InstrWrapC store name
     , SingI (ToT store)
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
  , InstrWrapC store name
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
     ( StoreOpC store name
     , InstrWrapC store name
     , SingI (ToT store)
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
