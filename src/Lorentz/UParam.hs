{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Lorentz.UParam
  ( UParam (..)
  , EntryPointKind
  , type (?:)

    -- * Construction
  , mkUParam

    -- * Pattern-matching
  , EntryPointsImpl
  , UParamFallback
  , EntryPointLookupError (..)
  , CaseUParam
  , caseUParam
  , caseUParamT
  , uparamFallbackFail

    -- * Constraints
  , LookupEntryPoint
  , RequireUniqueEntryPoints

    -- * Conversion from ADT
  , uparamFromAdt
  , UParamLinearize
  , UParamLinearized

    -- * Internals used for entrypoint-wise migrations
  , unwrapUParam
  ) where

import qualified Data.Kind as Kind
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (Label)
import Data.Vinyl.TypeLevel (type (++))
import qualified Fcf
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (KnownSymbol, Symbol)

import Lorentz.ADT
import Lorentz.Base
import Lorentz.Coercions
import Lorentz.Constraints
import Lorentz.Errors
import Lorentz.Instr as L
import Lorentz.Macro
import Michelson.Interpret.Pack
import Michelson.Text
import Michelson.Typed
import Util.Type
import Util.TypeLits
import Util.TypeTuple

-- | An entry point is described by two types: its name and type of argument.
type EntryPointKind = (Symbol, Kind.Type)

-- | A convenient alias for type-level name-something pair.
type (n :: Symbol) ?: (a :: k) = '(n, a)

-- | Encapsulates parameter for one of entry points.
-- It keeps entry point name and corresponding argument serialized.
--
-- In Haskell world, we keep an invariant of that contained value relates
-- to one of entry points from @entries@ list.
newtype UParam (entries :: [EntryPointKind]) = UParamUnsafe (MText, ByteString)
  deriving stock (Generic, Eq, Show)
  deriving anyclass (IsoValue)

----------------------------------------------------------------------------
-- Common type-level stuff
----------------------------------------------------------------------------

-- | Get type of entry point argument by its name.
type family LookupEntryPoint (name :: Symbol) (entries :: [EntryPointKind])
             :: Kind.Type where
  LookupEntryPoint name ('(name, a) ': _) = a
  LookupEntryPoint name (_ ': entries) = LookupEntryPoint name entries
  LookupEntryPoint name '[] =
    TypeError ('Text "Entry point " ':<>: 'ShowType name ':<>:
               'Text " in not in the entry points list")

-- | Ensure that given entry points do no contain duplicated names.
type family RequireUniqueEntryPoints (entries :: [EntryPointKind])
             :: Constraint where
  RequireUniqueEntryPoints entries =
    RequireAllUnique "entry point" (Fcf.Eval (Fcf.Map Fcf.Fst entries))

----------------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------------

-- | Construct a 'UParam' safely.
mkUParam
  :: ( KnownSymbol name, IsoValue a, KnownValue a, NoOperation a, NoBigMap a
     , LookupEntryPoint name entries ~ a
     , RequireUniqueEntryPoints entries
     )
  => Label name -> a -> UParam entries
mkUParam label (a :: a) =
  forbiddenOp @(ToT a) $ forbiddenBigMap @(ToT a) $
  UParamUnsafe (labelToMText label, packValue' (toVal a))

-- Example
----------------------------------------------------------------------------

type MyEntryPoints =
  [ "add" ?: Integer
  , "reset" ?: ()
  ]

_mkParamSample1 :: UParam MyEntryPoints
_mkParamSample1 = mkUParam #add 5

----------------------------------------------------------------------------
-- Pattern-matching
----------------------------------------------------------------------------

-- | Helper instruction which extracts content of 'UParam'.
unwrapUParam :: UParam entries : s :-> (MText, ByteString) : s
unwrapUParam = coerce_

-- | Wrapper for a single "case" branch.
data CaseClauseU inp out (entry :: EntryPointKind) where
  CaseClauseU :: (arg : inp) :-> out -> CaseClauseU inp out '(name, arg)

instance (name ~ name', body ~ ((arg : inp) :-> out)) =>
         CaseArrow name' body (CaseClauseU inp out '(name, arg)) where
  (/->) _ = CaseClauseU

-- | Errors related to entry point lookup.
data EntryPointLookupError
  = NoSuchEntryPoint MText
  | ArgumentUnpackFailed
  deriving stock (Generic)

deriveCustomError ''EntryPointLookupError

-- | Implementations of some entry points.
--
-- Note that this thing inherits properties of 'Rec', e.g. you can
-- @Data.Vinyl.Core.rappend@ implementations for two entry point sets
-- when assembling scattered parts of a contract.
type EntryPointsImpl inp out entries =
  Rec (CaseClauseU inp out) entries

-- | An action invoked when user-provided entry point is not found.
type UParamFallback inp out = ((MText, ByteString) : inp) :-> out

-- | Default implementation for 'UParamFallback', simply reports an error.
uparamFallbackFail :: UParamFallback inp out
uparamFallbackFail =
  car # failUsingArg @EntryPointLookupError #cNoSuchEntryPoint

-- | Make up a "case" over entry points.
class CaseUParam (entries :: [EntryPointKind]) where
  -- | Pattern-match on given @UParam entries@.
  --
  -- You have to provide all case branches and a fallback action on case
  -- when entry point is not found.
  --
  -- This function is unsafe because it does not make sure at type-level
  -- that entry points' names do not repeat.
  caseUParamUnsafe
    :: Rec (CaseClauseU inp out) entries
    -> UParamFallback inp out
    -> (UParam entries : inp) :-> out

instance CaseUParam '[] where
  caseUParamUnsafe RNil fallback = unwrapUParam # fallback

instance ( KnownSymbol name
         , CaseUParam entries
         , KnownValue arg, NoOperation arg, NoBigMap arg
         ) =>
         CaseUParam ((name ?: arg) ': entries) where
  caseUParamUnsafe (CaseClauseU clause :& clauses) fallback =
    dup # unwrapUParam # car #
    push (mkMTextUnsafe $ symbolValT' @name) # eq #
    if_ (unwrapUParam # cdr # unpack # assertSome ArgumentUnpackFailed # clause)
        (cutUParamEntry # caseUParamUnsafe clauses fallback)
    where
      cutUParamEntry :: UParam (e : es) : s :-> UParam es : s
      cutUParamEntry = coerce_

-- | Pattern-match on given @UParam entries@.
--
-- You have to provide all case branches and a fallback action on case
-- when entry point is not found.
caseUParam
  :: (CaseUParam entries, RequireUniqueEntryPoints entries)
  => Rec (CaseClauseU inp out) entries
  -> UParamFallback inp out
  -> (UParam entries : inp) :-> out
caseUParam = caseUParamUnsafe

-- | Like 'caseUParam', but accepts a tuple of clauses, not a 'Rec'.
caseUParamT
  :: ( clauses ~ Rec (CaseClauseU inp out) entries
     , RecFromTuple clauses
     , CaseUParam entries
     )
  => IsoRecTuple clauses
  -> UParamFallback inp out
  -> (UParam entries : inp) :-> out
caseUParamT clauses fallback = caseUParamUnsafe (recFromTuple clauses) fallback

-- Example
----------------------------------------------------------------------------

_caseSample :: UParam MyEntryPoints : s :-> Integer : s
_caseSample = caseUParamT
  ( #add /-> nop
  , #reset /-> L.drop @() # push 0
  ) uparamFallbackFail

----------------------------------------------------------------------------
-- ADT conversion
----------------------------------------------------------------------------

{- @martoon: I actually hope no one will use this capability,
   it's here primarily because in other places we also use Generic stuff.

   Representation with type-level list and a datatype made polymorhpic over
   it seems more powerful than Generics.
   1. It's simpler to implement features for it. No extra boilerplate.
   2. 'Data.Vinyl' provides many useful utilities to work with such things.
   3. You are not such constrained in selecting names of entry points as when
      they come from constructor names.
-}

-- | Make up 'UParam' from ADT sum.
--
-- Entry points template will consist of
-- @(constructorName, constructorFieldType)@ pairs.
-- Each constructor is expected to have exactly one field.
uparamFromAdt
  :: UParamLinearize up
  => up -> UParam (UParamLinearized up)
uparamFromAdt = adtToRec . G.from

-- | Constraint required by 'uparamFromAdt'.
type UParamLinearize p = (Generic p, GUParamLinearize (G.Rep p))

-- | Entry points template derived from given ADT sum.
type UParamLinearized p = GUParamLinearized (G.Rep p)

-- | Generic traversal for conversion between ADT sum and 'UParam'.
class GUParamLinearize (x :: Kind.Type -> Kind.Type) where
  type GUParamLinearized x :: [(Symbol, Kind.Type)]
  adtToRec :: x p -> UParam (GUParamLinearized x)

instance GUParamLinearize x => GUParamLinearize (G.D1 i x) where
  type GUParamLinearized (G.D1 i x) = GUParamLinearized x
  adtToRec = adtToRec . G.unM1

instance (GUParamLinearize x, GUParamLinearize y) => GUParamLinearize (x :+: y) where
  type GUParamLinearized (x :+: y) = GUParamLinearized x ++ GUParamLinearized y
  adtToRec = \case
    G.L1 x -> let UParamUnsafe up = adtToRec x in UParamUnsafe up
    G.R1 y -> let UParamUnsafe up = adtToRec y in UParamUnsafe up

instance (KnownSymbol name, IsoValue a, KnownValue a, NoOperation a, NoBigMap a) =>
         GUParamLinearize (G.C1 ('G.MetaCons name _1 _2) (G.S1 si (G.Rec0 a))) where
  type GUParamLinearized (G.C1 ('G.MetaCons name _1 _2) (G.S1 si (G.Rec0 a))) =
    '[ '(name, a) ]

  adtToRec (G.M1 (G.M1 (G.K1 a))) = UParamUnsafe
    ( symbolToMText @name
    , forbiddenOp @(ToT a) $ forbiddenBigMap @(ToT a) $
        packValue' $ toVal a
    )

instance
    TypeError ('Text "UParam linearization requires exactly one field \
                    \in each constructor") =>
    GUParamLinearize (G.C1 i G.U1) where
  type GUParamLinearized (G.C1 i G.U1) =
    TypeError ('Text "Bad linearized ADT")
  adtToRec = error "impossible"

instance
    TypeError ('Text "UParam linearization requires exactly one field \
                    \in each constructor") =>
    GUParamLinearize (G.C1 i (x :*: y)) where
  type GUParamLinearized (G.C1 i (x :*: y)) =
    TypeError ('Text "Bad linearized ADT")
  adtToRec = error "impossible"
