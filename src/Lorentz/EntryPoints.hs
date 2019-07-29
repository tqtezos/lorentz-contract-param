-- | Utilities for declaring and documenting entry points.
module Lorentz.EntryPoints
  ( DEntryPoint (..)
  , PlainEntryPointsKind
  , diEntryPointToMarkdown
  , DEntryPointArg (..)
  , DeriveCtorFieldDoc (..)
  , ParamBuildingStep (..)
  , clarifyParamBuildingSteps
  , entryCase
  ) where

import qualified Data.Kind as Kind
import Data.Vinyl.Core (Rec(..), rappend)
import Fmt (Builder, build)
import GHC.Generics ((:+:))
import qualified GHC.Generics as G
import GHC.TypeLits (KnownSymbol, symbolVal)

import Lorentz.ADT
import Lorentz.Base
import Lorentz.Doc
import Michelson.Typed.Doc
import Michelson.Typed.DocScan
import Michelson.Typed.Haskell.Doc
import Michelson.Typed.Haskell.Instr
import Michelson.Typed.Haskell.Instr.Helpers
import Util.Markdown
import Util.TypeTuple

-- | Gathers information about single entry point.
--
-- We assume that entry points might be of different kinds,
-- which is designated by phantom type parameter.
-- For instance, you may want to have several groups of entry points
-- corresponding to various parts of a contract - specifying different @kind@
-- type argument for each of those groups will allow you defining different
-- 'DocItem' instances with appropriate custom descriptions for them.
data DEntryPoint (kind :: Kind.Type) = DEntryPoint Text DocBlock

-- | Default implementation of 'docItemToMarkdown' for entry points.
diEntryPointToMarkdown :: HeaderLevel -> DEntryPoint level -> Builder
diEntryPointToMarkdown lvl (DEntryPoint name block) =
  mdSeparator <>
  mdHeader lvl (mdTicked $ build name) <>
  docBlockToMarkdown (nextHeaderLevel lvl) block

-- | Default value for 'DEntryPoint' type argument.
data PlainEntryPointsKind

instance DocItem (DEntryPoint PlainEntryPointsKind) where
  type DocItemPosition (DEntryPoint PlainEntryPointsKind) = 1000
  docItemSectionName = Just "Entry points"
  docItemToMarkdown = diEntryPointToMarkdown

-- | During incremental assembly of parameter building steps -
-- current representation of parameter.
type CurrentParam = Builder

-- | Describes a parameter building step.
--
-- This can be wrapping into (Haskell) constructor, or a more complex
-- transformation.
data ParamBuildingStep = ParamBuildingStep
  { pbsEnglish :: Builder
    -- ^ Plain english description of this step.
  , pbsHaskell :: CurrentParam -> Builder
    -- ^ How to construct parameter in Haskell code.
  , pbsMichelson :: CurrentParam -> Builder
    -- ^ How to construct parameter working on raw Michelson.
  }

-- | Describes argument of an entry point.
data DEntryPointArg = DEntryPointArg
  { epaArg :: Maybe DType
    -- ^ Argument of the entry point. Pass 'Nothing' if no argument is required.
  , epaBuilding :: [ParamBuildingStep]
    -- ^ Describes a way to lift an entry point argument into full parameter
    -- which can be passed to the contract.
    --
    -- Steps are supposed to be applied in the order in which they are given.
    -- E.g. suppose that an entry point is called as @Run (Service1 arg)@;
    -- then the first step should describe wrapping into @Service1@ constructor,
    -- and the second step should be about wrapping into @Run@ constructor.
  }

-- | Go over contract code and update every occurrence of 'DEntryPointArg'
-- documentation item, adding the given step to its "how to build parameter"
-- description.
clarifyParamBuildingSteps :: ParamBuildingStep -> (inp :-> out) -> (inp :-> out)
clarifyParamBuildingSteps pbs (I instr) = I $
  modifyInstrDoc (\di -> di{ epaBuilding = epaBuilding di ++ [pbs] }) instr

instance DocItem DEntryPointArg where
  type DocItemPosition DEntryPointArg = 20
  docItemSectionName = Nothing
  docItemDependencies (DEntryPointArg mdty _) =
    [SomeDocDefinitionItem dty | Just dty <- pure mdty]
  docItemToMarkdown _ (DEntryPointArg mdty psteps) =
    mconcat . Prelude.map (<> "\n\n") $
      [ mdSubsection "Parameter" $
          case mdty of
            Nothing -> "none (pass unit)"
            Just (DType dty) -> typeDocMdReference dty (WithinParens False)

      , mdSpoiler "How to call this entry point" . mconcat $
          [ "\n0. Construct parameter for the entry point.\n"
          , mconcat . Prelude.intersperse "\n" $
              psteps <&> \ParamBuildingStep{..} ->
                mconcat . Prelude.intersperse "\n" $
                [ -- Markdown re-enumerates enumerated lists automatically
                  "1. " <> pbsEnglish
                , "    + " <>
                  mdSubsection "In Haskell" (mdTicked $ pbsHaskell "·")
                , "    + " <>
                  mdSubsection "In Michelson" (mdTicked $ pbsMichelson "·")
                ]
          , "\n\nPass resulting value as parameter to the contract.\n"
          ]
      ]

-- | Pick a type documentation from 'CtorField'.
class DeriveCtorFieldDoc (cf :: CtorField) where
  deriveCtorFieldDoc :: Maybe DType
instance DeriveCtorFieldDoc 'NoFields where
  deriveCtorFieldDoc = Nothing
instance TypeHasDoc ty => DeriveCtorFieldDoc ('OneField ty) where
  deriveCtorFieldDoc = Just $ DType (Proxy @ty)

-- | Add necessary documentation to entry points.
documentEntryPoints
  :: forall a kind inp out.
     DocumentEntryPoints kind a
  => Rec (CaseClauseL inp out) (CaseClauses a)
  -> Rec (CaseClauseL inp out) (CaseClauses a)
documentEntryPoints = gDocumentEntryPoints @kind @(G.Rep a) id

-- | Constraint for 'documentEntryPoints'.
type DocumentEntryPoints kind a =
  (Generic a, GDocumentEntryPoints kind (G.Rep a))

-- | Traverse entry points and add parameter building step (which describes
-- necessity to wrap parameter into some constructor of the given datatype)
-- to all parameters described within given code.
class GDocumentEntryPoints (kind :: Kind.Type) (x :: Kind.Type -> Kind.Type) where
  -- | Add corresponding parameter building step.
  --
  -- First argument is accumulator for Michelson description of the building step.
  gDocumentEntryPoints
    :: (Builder -> Builder)
    -> Rec (CaseClauseL inp out) (GCaseClauses x)
    -> Rec (CaseClauseL inp out) (GCaseClauses x)

instance GDocumentEntryPoints kind x => GDocumentEntryPoints kind (G.D1 i x) where
  gDocumentEntryPoints = gDocumentEntryPoints @kind @x

instance ( GDocumentEntryPoints kind x, GDocumentEntryPoints kind y
         , RSplit (GCaseClauses x) (GCaseClauses y)
         ) =>
         GDocumentEntryPoints kind (x :+: y) where
  gDocumentEntryPoints michDesc clauses =
    let (lclauses, rclauses) = rsplit @CaseClauseParam @(GCaseClauses x) clauses
    in gDocumentEntryPoints @kind @x
         (\a -> michDesc $ "Left (" <> a <> ")")
         lclauses
       `rappend`
       gDocumentEntryPoints @kind @y
         (\a -> michDesc $ "Right (" <> a <> ")")
         rclauses

instance ( 'CaseClauseParam ctor cf ~ GCaseBranchInput ctor x
         , KnownSymbol ctor
         , DocItem (DEntryPoint kind)
         , DeriveCtorFieldDoc cf
         ) =>
         GDocumentEntryPoints kind (G.C1 ('G.MetaCons ctor _1 _2) x) where
  gDocumentEntryPoints michDesc (CaseClauseL clause :& RNil) =
    let entryPointName = toText $ symbolVal (Proxy @ctor)
        psteps = ParamBuildingStep
          { pbsEnglish = "Wrap into " <> mdTicked (build entryPointName) <> " constructor."
          , pbsHaskell = \p -> build entryPointName <> " (" <> p <> ")"
          , pbsMichelson = michDesc
          }
        addDoc instr =
          clarifyParamBuildingSteps psteps $
          docGroup (SomeDocItem . DEntryPoint @kind entryPointName) $
          doc (DEntryPointArg (deriveCtorFieldDoc @cf) []) # instr
    in CaseClauseL (addDoc clause) :& RNil

-- | Like 'caseT', to be used for pattern-matching on parameter.
--
-- Modifies documentation accordingly. Including description of
-- entrypoints' arguments, thus for them you will need to supply
-- 'TypeHasDoc' instance.
entryCase
  :: forall dt entryPointKind out inp clauses.
     ( CaseTC dt out inp clauses
     , DocumentEntryPoints entryPointKind dt
     )
  => Proxy entryPointKind -> IsoRecTuple clauses -> dt & inp :-> out
entryCase _ =
  case_ . documentEntryPoints @dt @entryPointKind . recFromTuple
