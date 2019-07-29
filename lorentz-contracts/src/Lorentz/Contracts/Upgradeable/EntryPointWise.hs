module Lorentz.Contracts.Upgradeable.EntryPointWise
  ( EntryPointImpl
  , EpwFallback
  , EpwContract (..)
  , EpwCaseClause (..)
  , mkEpwContract
  , mkEpwContractT
  , epwFallbackFail
  , (/==>)
  , removeEndpoint
  , EpwDocumented (..)
  ) where

import Lorentz
import Prelude (fmap)

import Data.Vinyl.Core (Rec)
import Data.Vinyl.Derived (Label(..))
import Fmt (build)

import Lorentz.Contracts.Upgradeable.Common
import Lorentz.UStore.Common
import Util.Markdown
import Util.TypeLits
import Util.TypeTuple

-- | This data type represents the new contract code and migrations necessary
--   to upgrade the contract endpoints to the new version.
data EpwContract interface store = EpwContract
  { epwServe :: ContractCode
  -- ^ `epwServe` does  the dispatching logic and is assumed to be used for
  --   the `code` lambda of the upgradeable contract.

  , epwCodeMigrations :: [MigrationScript]
  -- ^ `epwCodeMigrations` is a list of packed migrations the client ought to
  --   pass to the `EpwUpgrade` method in order to upgrade the implementation.
  }


-- | Creates the EpwContract data structure from a Rec of case clauses
mkEpwContract
  :: forall (interface :: [EntryPointKind]) store.
  ( CodeMigrations interface
  , GetUStoreKey store "code" ~ MText
  , GetUStoreValue store "code" ~ EntryPointImpl store
  , GetUStoreField store "fallback" ~ EpwFallback store
  )
  => Rec (EpwCaseClause store) interface
  -> EpwFallback store
  -> EpwContract interface store
mkEpwContract entries fallback = EpwContract
  { epwServe = do
      unpair
      coerce_ @UParameter @(UParam interface)
      dip (coerce_ @UStore_ @(UStore store))
      caseUParamUnsafe'
      unpair
      dip (coerce_ @(UStore store) @UStore_)
      pair
  , epwCodeMigrations =
      fmap (coercing_ @UStore_ @(UStore store)) $
        (push fallback # ustoreSetField #fallback) : mkMigrations entries
  }

-- | Like 'mkEpwContract', but accepts a tuple of clauses, not a 'Rec'.
mkEpwContractT
  :: forall clauses (interface :: [EntryPointKind]) store.
  ( clauses ~ Rec (EpwCaseClause store) interface
  , RecFromTuple clauses
  , CodeMigrations interface
  , GetUStoreKey store "code" ~ MText
  , GetUStoreValue store "code" ~ EntryPointImpl store
  , GetUStoreField store "fallback" ~ EpwFallback store
  )
  => IsoRecTuple clauses
  -> EpwFallback store
  -> EpwContract interface store
mkEpwContractT clauses fallback = mkEpwContract (recFromTuple clauses) fallback

-- | A helper type that defines an entry point that receives
--   an unpacked argument
type TypedEntryPointImpl arg store =
  Lambda (arg, UStore store) ([Operation], UStore store)

-- | A helper type that defines an entry point that receives
--   a packed argument, i.e. it's basically an unpack instruction
--   followed by a TypedEntryPoint code
type EntryPointImpl store =
  Lambda (ByteString, UStore store) ([Operation], UStore store)

-- | A helper type that defines a function being called in case
--   no implementation matches the requested entry point
type EpwFallback store =
  Lambda ((MText, ByteString), UStore store) ([Operation], UStore store)

-- | Pretends that the top item of the stack was differently typed
coercing_
  :: forall a b s. (Coercible_ a b)
  => (b ': s :-> b ': s) -> (a ': s :-> a ': s)
coercing_ f = coerce_ @a @b # f # coerce_ @b @a

-- | A data type representing a full case clause with the name
--   and implementation of an entry point.
data EpwCaseClause store (entry :: EntryPointKind) where
  EpwCaseClause
    :: TypedEntryPointImpl arg store
    -> EpwCaseClause store '(name, arg)

(/==>)
  :: Label name
  -> Lambda (arg, UStore store) ([Operation], UStore store)
  -> EpwCaseClause store '(name, arg)
(/==>) _ = EpwCaseClause
infixr 0 /==>

-- | A greatly simplified version of UParam lookup code.
--
--   While it does not provide the same safety guarantees as UParam's lookup,
--   it does a map search instead of a linear search, and thus it may consume
--   less gas in practice.
caseUParamUnsafe'
  :: forall store (entries :: [EntryPointKind]).
  ( GetUStoreKey store "code" ~ MText
  , GetUStoreValue store "code" ~ EntryPointImpl store
  , GetUStoreField store "fallback" ~ EpwFallback store
  )
  => '[UParam entries, UStore store] :-> '[([Operation], UStore store)]
caseUParamUnsafe' = do
  dup
  unwrapUParam
  unpair
  dip (duupX @3)
  ustoreGet #code
  if IsSome
  then dip (dip (drop) # pair) # swap # exec
  else do
    drop
    dip (ustoreGetField #fallback # swap)
    unwrapUParam
    pair
    exec

-- | Default implementation for 'EpwFallback' reports an error just like its
--   UParam counterpart
epwFallbackFail :: EpwFallback store
epwFallbackFail =
  car # car # failUsingArg @EntryPointLookupError #cNoSuchEntryPoint

-- | These functions create the code blocks one has to supply in order
--   upgrade a contract. These code blocks write the code of the contract
--   to a submap of UStore. Code migrations _do not delete_ the old code
--   blocks from UStore, so would still be possible to call the old entry
--   points manually after applying migrations.
class CodeMigrations (entries :: [EntryPointKind]) where
  mkMigrations
    :: forall store.
    ( GetUStoreKey store "code" ~ MText
    , GetUStoreValue store "code" ~ EntryPointImpl store
    )
    =>  Rec (EpwCaseClause store) entries
    -> ['[UStore store] :-> '[UStore store]]

instance
  ( CodeMigrations entries
  , KnownSymbol name
  , KnownValue arg, NoOperation arg, NoBigMap arg
  )
  => CodeMigrations ((name ?: arg) ': entries) where
    mkMigrations (EpwCaseClause impl :& clauses) =
      (push untypedLambda # push (fieldNameToMText @name) # ustoreInsert #code)
      : mkMigrations clauses
      where
        untypedLambda = do
          unpair
          unpack @arg
          assertSome ArgumentUnpackFailed
          pair
          impl

instance CodeMigrations '[] where
  mkMigrations _ = []

-- | Removes an endpoint from the #code submap
removeEndpoint
  :: forall store name s.
  ( KnownSymbol name
  , GetUStoreKey store "code" ~ MText
  )
  => Label name
  -> UStore store ': s :-> UStore store ': s
removeEndpoint _ = do
  push $ fieldNameToMText @name
  ustoreDelete #code

-- | Helper for documenting entrypoints with EPW interface.
class EpwDocumented (entries :: [EntryPointKind]) where
  -- | Make up documentation for given entry points.
  --
  -- As result you get a fake contract from which you can later build desired
  -- documentation. Although, you may want to add contract name and
  -- description first.
  epwDocument
    :: Rec (EpwCaseClause store) entries
    -> Lambda () ()

instance EpwDocumented '[] where
  epwDocument RNil = nop

instance (NoOperation a, NoBigMap a, KnownSymbol name, EpwDocumented es) =>
         EpwDocumented ('(name, a) ': es) where
  epwDocument (EpwCaseClause code :& es) =
    let documentedCode = clarifyParamBuildingSteps pstep code
    in push True # if_ (fakeCoerce # documentedCode # fakeCoerce) (epwDocument es)
    where
      ctor = build $ symbolValT' @name
      pstep = ParamBuildingStep
        { pbsEnglish =
            "Wrap into *UParam* as " <> mdTicked ctor <> " entry point."
        , pbsHaskell =
            \a -> "mkUParam #" <> ctor <> " (" <> a <> ")"
        , pbsMichelson =
            \a -> "Pair \"" <> ctor <> "\" (pack (" <> a <> "))"
        }
