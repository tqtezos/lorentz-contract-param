{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Composability helper for 'UStore'.
module Lorentz.UStore.Lift
  ( liftUStore
  , unliftUStore

    -- * Internals
  , UStoreFieldsAreUnique
  ) where

import Data.Vinyl.Derived (Label)
import Data.Type.Bool (If)
import qualified Data.Kind as Kind
import GHC.TypeLits (Symbol, TypeError, ErrorMessage (..))
import Data.Vinyl.TypeLevel (type (++))
import GHC.Generics (type (:+:), type (:*:))
import qualified GHC.Generics as G

import Lorentz.Base
import Lorentz.Coercions
import Lorentz.Instr
import Lorentz.UStore.Types
import Michelson.Typed.Haskell
import Util.Type

-- | Get all fields names accessible in given 'UStore' template.
type UStoreFields (template :: Kind.Type) = GUStoreFields (G.Rep template)

type family GUStoreFields (x :: Kind.Type -> Kind.Type) :: [Symbol] where
  -- We are not oblidged to fail in case of bad template - this will be handled
  -- in other operations with 'UStore' anyway.
  GUStoreFields (G.D1 _ x) = GUStoreFields x
  GUStoreFields (_ :+: _) = '[]
  GUStoreFields G.V1 = '[]
  GUStoreFields (G.C1 _ x) = GUStoreFields x
  GUStoreFields (x :*: y) = GUStoreFields x ++ GUStoreFields y
  GUStoreFields (G.S1 ('G.MetaSel ('Just fieldName) _ _ _) (G.Rec0 (_ |~> _))) =
    '[fieldName]
  GUStoreFields (G.S1 ('G.MetaSel ('Just fieldName) _ _ _) (G.Rec0 (UStoreField _))) =
    '[fieldName]
  GUStoreFields (G.S1 _ (G.Rec0 a)) =
    UStoreFields a

type UStoreFieldsAreUnique template = AllUnique (UStoreFields template)

type family RequireAllUniqueFields (template :: Kind.Type) :: Constraint where
  RequireAllUniqueFields template =
    If (UStoreFieldsAreUnique template)
       (() :: Constraint)
       (TypeError ('Text "Some field in template is duplicated"))
      -- TODO: if this ever fires for you and it's not clear which exact field
      -- is duplicated, please create a ticket to implement the corresponding
      -- logic.

-- | Lift an 'UStore' to another 'UStore' which contains all the entries
-- of the former under given field.
--
-- This function is not intended for use in migrations, only in normal
-- entry points.
--
-- Note that this function ensures that template of resulting store
-- does not contain inner nested templates with duplicated fields,
-- otherwise 'UStore' invariants could get broken.
liftUStore
  :: (Generic template, RequireAllUniqueFields template)
  => Label name
  -> UStore (GetFieldType template name) : s :-> UStore template : s
liftUStore _ = coerce_

-- | Unlift an 'UStore' to a smaller 'UStore' which is part of the former.
--
-- This function is not intended for use in migrations, only in normal
-- entry points.
--
-- Surprisingly, despite smaller 'UStore' may have extra entries,
-- this function is safe when used in contract code.
-- Truly, all getters and setters are still safe to use.
-- Also, there is no way for the resulting small @UStore@ to leak outside
-- of the contract since the only place where 'big_map' can appear
-- is contract storage, so this small @UStore@ can be either dropped
-- or lifted back via 'liftUStore' to appear as part of the new contract's state.
--
-- When this function is run as part of standalone instructions sequence,
-- not as part of contract code (e.g. in tests), you may get an @UStore@
-- with entries not inherent to it.
unliftUStore
  :: (Generic template)
  => Label name
  -> UStore template : s :-> UStore (GetFieldType template name) : s
unliftUStore _ = coerce_

-- Examples
----------------------------------------------------------------------------

data MyStoreTemplate = MyStoreTemplate
  { ints :: Integer |~> Integer
  } deriving stock Generic

data MyStoreTemplateBig = MyStoreTemplateBig
  { bool :: UStoreField Bool
  , substore :: MyStoreTemplate
  } deriving stock Generic

-- | This example demostrates a way to run an instruction, operating on small
-- 'UStore', so that it works on a larger 'UStore'.
_sampleWithMyStore
  :: ('[param, UStore MyStoreTemplate] :-> '[UStore MyStoreTemplate])
  -> ('[param, UStore MyStoreTemplateBig] :-> '[UStore MyStoreTemplateBig])
_sampleWithMyStore instr =
  dip (unliftUStore #substore) # instr # liftUStore #substore
