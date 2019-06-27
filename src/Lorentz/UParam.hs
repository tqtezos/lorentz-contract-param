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
  deriving stock (Generic)
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
