{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

-- | 'UStore' definition and common type-level stuff.
module Lorentz.UStore.Types
  ( -- * UStore and related type definitions
    UStore (..)
  , type (|~>)(..)
  , UStoreField (..)

    -- ** Type-lookup-by-name
  , GetUStoreKey
  , GetUStoreValue
  , GetUStoreField

   -- * Internals
  , ElemSignature (..)
  , GetUStore
  , MSKey
  , MSValue
  , FSValue
  ) where

import Data.Default (Default)
import qualified Data.Kind as Kind
import Data.Type.Equality (type (==))
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

import Lorentz.Polymorphic
import Michelson.Typed.Haskell.Value
import Util.Type

-- | Gathers multple fields and 'BigMap's under one object.
--
-- Type argument of this datatype stands for a "store template" -
-- a datatype with one constructor and multiple fields, each containing
-- an object of type 'UStoreField' or '|~>' and corresponding to single
-- virtual field or 'BigMap' respectively.
-- It's also possible to parameterize it with a larger type which is
-- a product of types satisfying the above property.
--
-- Inside it keeps only one 'BigMap' thus not violating Michelson limitations.
newtype UStore a = UStore { unUStore :: BigMap ByteString ByteString }
  deriving stock (Eq, Show)
  deriving newtype (Default, Semigroup, Monoid, IsoValue,
                    MemOpHs, GetOpHs, UpdOpHs)

-- | Describes one virtual big map in the storage.
newtype k |~> v = UStoreSubMap (Map k v)
  deriving stock (Show, Eq)
  deriving newtype (Default)

newtype UStoreField v = UStoreField v
  deriving stock (Show, Eq)

-- Type-safe lookup magic
----------------------------------------------------------------------------

{- Again we use generic magic to implement methods for 'Store'
(and thus 'Store' type constructor accepts a datatype, not a type-level list).

There are two reasons for this:

1. This gives us expected balanced tree of 'Or's for free.

2. This allows us selecting a map by field name, not by
e.g. type of map value. This is subjective, but looks like a good thing
for me (@martoon). On the other hand, it prevents us from sharing the
same interface between maps and 'Store'.

-}

-- | What was found on lookup by constructor name.
--
-- This keeps either type arguments of '|~>' or 'UStoreField'.
data ElemSignature
  = MapSignature Kind.Type Kind.Type
  | FieldSignature Kind.Type

-- Again, we will use these getters instead of binding types within
-- 'MapSignature' using type equality because getters does not produce extra
-- compile errors on "field not found" cases.
type family MSKey ms where
  MSKey ('MapSignature k _) = k
  MSKey ('FieldSignature _) =
    TypeError ('Text "Expected UStore submap, but field was referred")
type family MSValue ms where
  MSValue ('MapSignature _ v) = v
  MSValue ('FieldSignature _) =
    TypeError ('Text "Expected UStore submap, but field was referred")
type family FSValue ms where
  FSValue ('FieldSignature v) = v
  FSValue ('MapSignature _ _) =
    TypeError ('Text "Expected UStore field, but submap was referred")

-- | Get map signature from the constructor with a given name.
type GetUStore name a = MERequireFound name a (GLookupStore name (G.Rep a))

type family MERequireFound
  (name :: Symbol)
  (a :: Kind.Type)
  (mlr :: Maybe ElemSignature)
    :: ElemSignature where
  MERequireFound _ _ ('Just ms) = ms
  MERequireFound name a 'Nothing = TypeError
    ('Text "Failed to find plain field or submap in store template: datatype `"
     ':<>: 'ShowType a ':<>: 'Text "` has no field " ':<>: 'ShowType name)

type family GLookupStore (name :: Symbol) (x :: Kind.Type -> Kind.Type)
              :: Maybe ElemSignature where
  GLookupStore name (G.D1 _ x) = GLookupStore name x
  GLookupStore _ (_ :+: _) =
    TypeError ('Text "Templates used in UStore should have only one constructor")
  GLookupStore _ G.V1 =
    TypeError ('Text "No constructors in UStore template")

  GLookupStore name (G.C1 _ x) = GLookupStore name x

  GLookupStore name (x :*: y) = LSMergeFound name (GLookupStore name x)
                                                  (GLookupStore name y)

  -- When we encounter a field there are three cases we are interested in:
  -- 1. This field has type '|~>'. Then we check its name and return 'Just'
  -- with all required info on match, and 'Nothing' otherwise.
  -- 2. This field has type 'UStoreField'. We act in the same way
  -- as for '|~>', attaching 'ThePlainFieldKey' as key.
  -- 3. This field type is a different one. Then we expect this field to store
  -- '|~>' or 'UStoreField' somewhere deeper and try to find it there.
  GLookupStore name (G.S1 ('G.MetaSel mFieldName _ _ _) (G.Rec0 (k |~> v))) =
    Guard ('Just name == mFieldName) ('MapSignature k v)
  GLookupStore name (G.S1 ('G.MetaSel mFieldName _ _ _) (G.Rec0 (UStoreField v))) =
    Guard ('Just name == mFieldName) ('FieldSignature v)

  GLookupStore name (G.S1 _ (G.Rec0 a)) =
    GLookupStore name (G.Rep a)

  GLookupStore _ G.U1 = 'Nothing

type family LSMergeFound (name :: Symbol)
  (f1 :: Maybe ElemSignature) (f2 :: Maybe ElemSignature)
  :: Maybe ElemSignature where
  LSMergeFound _ 'Nothing 'Nothing = 'Nothing
  LSMergeFound _ ('Just ms) 'Nothing = 'Just ms
  LSMergeFound _ 'Nothing ('Just ms) = 'Just ms
  -- It's possible that there are two constructors with the same name,
  -- because main template pattern may be a sum of smaller template
  -- patterns with same constructor names.
  LSMergeFound ctor ('Just _) ('Just _) = TypeError
    ('Text "Found more than one constructor matching " ':<>: 'ShowType ctor)

-- | Get type of submap key.
type GetUStoreKey store name = MSKey (GetUStore name store)

-- | Get type of submap value.
type GetUStoreValue store name = MSValue (GetUStore name store)

-- | Get type of plain field.
type GetUStoreField store name = FSValue (GetUStore name store)
