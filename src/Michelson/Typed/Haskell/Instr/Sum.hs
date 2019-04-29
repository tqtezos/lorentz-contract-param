{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Instructions working on sum types derived from Haskell ones.
module Michelson.Typed.Haskell.Instr.Sum
  ( InstrWrapC
  , InstrCaseC
  , instrWrap
  , instrCase
  , (//->)

  , CaseClauseParam (..)
  , CaseClause (..)
  , CaseClauses

  , CtorField (..)
  , AppendCtorField
  , AppendCtorFieldAxiom
  , appendCtorFieldAxiom
  ) where

import qualified Data.Kind as Kind
import Data.Singletons (SingI (..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Type.Bool (If, type (||))
import Data.Constraint (Dict (..))
import Data.Type.Equality (type (==))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (Label)
import Data.Vinyl.TypeLevel (type (++))
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (AppendSymbol, ErrorMessage(..), Symbol, TypeError)
import Named ((:!))

import Michelson.Typed.Haskell.Instr.Helpers
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Instr
import Michelson.Typed.T
import Michelson.Typed.Value
import Util.TypeTuple

-- Constructors lookup (helper)
----------------------------------------------------------------------------

-- | We support only two scenarious - constructor with one field and
-- without fields. Nonetheless, it's not that sad since for sum types
-- we can't even assign names to fields if there are many (the style
-- guide prohibits partial records).
data CtorField
  = OneField Kind.Type
  | NoFields

-- | Push field to stack, if any.
type family AppendCtorField (cf :: CtorField) (l :: [k]) :: [k] where
  AppendCtorField ('OneField t) (l :: [T]) = ToT t ': l
  AppendCtorField ('OneField t) (l :: [Kind.Type]) = t ': l
  AppendCtorField 'NoFields (l :: [T]) = l
  AppendCtorField 'NoFields (l :: [Kind.Type]) = l

-- | To use 'AppendCtorField' not only here for 'T'-based stacks, but also
-- later in Lorentz with 'Kind.Type'-based stacks we need the following property.
type AppendCtorFieldAxiom (cf :: CtorField) (st :: [Kind.Type]) =
  ToTs (AppendCtorField cf st) ~ AppendCtorField cf (ToTs st)

-- | Proof of 'AppendCtorFieldAxiom'.
appendCtorFieldAxiom
  :: ( AppendCtorFieldAxiom ('OneField Word) '[Int]
     , AppendCtorFieldAxiom 'NoFields '[Int]
     )
  => Dict (AppendCtorFieldAxiom cf st)
appendCtorFieldAxiom =
  -- In order to avoid a lot of boilerplate and burdening GHC type checker we
  -- write an unsafe code. Its correctness is tested in dummy constraints of
  -- this function.
  unsafeCoerce $ Dict @(AppendCtorFieldAxiom 'NoFields '[])

-- | Result of field or constructor lookup - entry type and path to it in
-- the tree.
data LookupNamedResult = LNR CtorField Path

type family LnrFieldType (lnr :: LookupNamedResult) where
  LnrFieldType ('LNR f _) = f
type family LnrBranch (lnr :: LookupNamedResult) :: Path where
  LnrBranch ('LNR _ p) = p

-- | Find constructor of some sum type by its name.
type GetNamed name a = LNRequireFound name a (GLookupNamed name (G.Rep a))

-- | Force result of 'GLookupNamed' to be 'Just'
type family LNRequireFound
  (name :: Symbol)
  (a :: Kind.Type)
  (mf :: Maybe LookupNamedResult)
    :: LookupNamedResult where
  LNRequireFound _ _ ('Just lnr) = lnr
  LNRequireFound name a 'Nothing = TypeError
    ('Text "Datatype " ':<>: 'ShowType a ':<>:
     'Text " has no constructor " ':<>: 'ShowType name)

type family GLookupNamed (name :: Symbol) (x :: Kind.Type -> Kind.Type)
          :: Maybe LookupNamedResult where
  GLookupNamed name (G.D1 _ x) = GLookupNamed name x
  GLookupNamed name (G.C1 ('G.MetaCons ctorName _ _) x) =
    If (name == ctorName || name == ("c" `AppendSymbol` ctorName))
       ('Just $ 'LNR (GExtractFields x) '[])
       'Nothing
  GLookupNamed name (x :+: y) =
    LNMergeFound name (GLookupNamed name x) (GLookupNamed name y)
  GLookupNamed _ G.V1 = 'Nothing

type family LNMergeFound
  (name :: Symbol)
  (f1 :: Maybe LookupNamedResult)
  (f2 :: Maybe LookupNamedResult)
    :: Maybe LookupNamedResult where
  LNMergeFound _ 'Nothing 'Nothing = 'Nothing
  LNMergeFound _ ('Just ('LNR a p)) 'Nothing = 'Just $ 'LNR a ('L ': p)
  LNMergeFound _ 'Nothing ('Just ('LNR a p)) = 'Just $ 'LNR a ('R ': p)
  LNMergeFound name ('Just _) ('Just _) = TypeError
    ('Text "Ambigous reference to datatype constructor: " ':<>: 'ShowType name)

type family GExtractFields (x :: Kind.Type -> Kind.Type)
          :: CtorField where
  GExtractFields (G.S1 _ x) = GExtractFields x
  GExtractFields (G.Rec0 a) = 'OneField a
  GExtractFields G.U1 = 'NoFields
  GExtractFields (_ :*: _) = TypeError
    ('Text "Cannot automatically lookup constructors having multiple fields"
     ':$$:
     'Text "Consider using tuple instead")
    --- ^ @martoon: Probably we will have no use cases for such scenario
    --- anyway, tuples seem to be not worse than separate fields in most cases.

----------------------------------------------------------------------------
-- Constructor wrapping instruction
----------------------------------------------------------------------------

-- | Wrap given element into a constructor with the given name.
--
-- Mentioned constructor must have only one field.
--
-- Since labels interpretable by "OverloadedLabels" extension cannot
-- start with capital latter, prepent constructor name with letter
-- "c" (see examples below).
instrWrap
  :: forall dt name entry st path.
     InstrWrapC dt name entry st path
  => Label name -> Instr (AppendCtorField entry st) (ToT dt ': st)
instrWrap _ = gInstrWrap @name @(G.Rep dt) @path @entry

type InstrWrapC dt name entry st path =
  ( IsoValue dt
  , GInstrWrap name (G.Rep dt)
      (LnrBranch (GetNamed name dt))
      (LnrFieldType (GetNamed name dt))
  , 'LNR entry path ~ GetNamed name dt
  , GValueType (G.Rep dt) ~ ToT dt
  )

-- | Generic traversal for 'instrWrap'.
class GIsoValue x =>
  GInstrWrap
    (name :: Symbol)
    (x :: Kind.Type -> Kind.Type)
    (path :: Path)
    (entryTy :: CtorField) where
  gInstrWrap
    :: GIsoValue x
    => Instr (AppendCtorField entryTy s) (GValueType x ': s)

instance GInstrWrap name x path e => GInstrWrap name (G.D1 i x) path e where
  gInstrWrap = gInstrWrap @name @x @path @e

instance (GInstrWrap name x path e, GIsoValue y, SingI (GValueType y)) =>
         GInstrWrap name (x :+: y) ('L ': path) e where
  gInstrWrap = gInstrWrap @name @x @path @e `Seq` LEFT

instance (GInstrWrap name y path e, GIsoValue x, SingI (GValueType x)) =>
         GInstrWrap name (x :+: y) ('R ': path) e where
  gInstrWrap = gInstrWrap @name @y @path @e `Seq` RIGHT

instance (IsoValue e) =>
         GInstrWrap name (G.C1 c (G.S1 i (G.Rec0 e))) '[] ('OneField e) where
  gInstrWrap = Nop

instance GInstrWrap name (G.C1 c G.U1) '[] 'NoFields where
  gInstrWrap = PUSH VUnit

-- Examples
----------------------------------------------------------------------------

data MyType
  = MyCtor Integer
  | ComplexThing ()
  | UselessThing ("field1" :! Text, "field2" :! ())
  deriving stock Generic
  deriving anyclass IsoValue

_wrapMyType1 :: Instr (ToT Integer ': s) (ToT MyType ': s)
_wrapMyType1 = instrWrap @MyType #cMyCtor

-- If "c" prefix is too annoying, one can construct labels without
-- OverloadedLabels extension.
_wrapMyType1' :: Instr (ToT Integer ': s) (ToT MyType ': s)
_wrapMyType1' = instrWrap @MyType @"MyCtor" fromLabel

_wrapMyType2 :: Instr (ToT () ': s) (ToT MyType ': s)
_wrapMyType2 = instrWrap @MyType #cComplexThing

_wrapMyType3 :: Instr (ToT (Text, ()) ': s) (ToT MyType ': s)
_wrapMyType3 = instrWrap @MyType #cUselessThing

data MyEnum = Case1 | Case2 | CaseN Integer | CaseDef
  deriving stock Generic
  deriving anyclass IsoValue

_wrapMyEnum1 :: Instr s (ToT MyEnum ': s)
_wrapMyEnum1 = instrWrap @MyEnum #cCase1

_wrapMyEnum2 :: Instr (ToT Integer ': s) (ToT MyEnum ': s)
_wrapMyEnum2 = instrWrap @MyEnum #cCaseN

----------------------------------------------------------------------------
-- "case" instruction
----------------------------------------------------------------------------

-- | Pattern-match on the given datatype.
instrCase
  :: forall dt out inp.
     InstrCaseC dt inp out
  => Rec (CaseClause inp out) (CaseClauses dt) -> Instr (ToT dt ': inp) out
instrCase = gInstrCase @(G.Rep dt)

type InstrCaseC dt inp out =
  ( IsoValue dt
  , GInstrCase (G.Rep dt)
  , GValueType (G.Rep dt) ~ ToT dt
  )

-- | In what different case branches differ - related constructor name and
-- input stack type which the branch starts with.
data CaseClauseParam = CaseClauseParam Symbol CtorField

-- | Type information about single case clause.
data CaseClause (inp :: [T]) (out :: [T]) (param :: CaseClauseParam) where
  CaseClause :: Instr (AppendCtorField x inp) out -> CaseClause inp out ('CaseClauseParam ctor x)

-- | Lift an instruction to case clause.
--
-- You should write out constructor name corresponding to the clause
-- explicitly. Prefix constructor name with "c" letter, otherwise
-- your label will not be recognized by Haskell parser.
-- Passing constructor name can be circumvented but doing so is not recomended
-- as mentioning contructor name improves readability and allows avoiding
-- some mistakes.
(//->)
  :: Label ("c" `AppendSymbol` ctor)
  -> Instr (AppendCtorField x inp) out
  -> CaseClause inp out ('CaseClauseParam ctor x)
(//->) _ = CaseClause
infixr 8 //->

-- | List of 'CaseClauseParam's required to pattern match on the given type.
type CaseClauses a = GCaseClauses (G.Rep a)

-- | Generic traversal for 'instrWrap'.
class GIsoValue x => GInstrCase (x :: Kind.Type -> Kind.Type) where

  type GCaseClauses x :: [CaseClauseParam]

  gInstrCase
    :: GIsoValue x
    => Rec (CaseClause inp out) (GCaseClauses x) -> Instr (GValueType x ': inp) out

instance GInstrCase x => GInstrCase (G.D1 i x) where
  type GCaseClauses (G.D1 i x) = GCaseClauses x
  gInstrCase = gInstrCase @x

instance (GInstrCase x, GInstrCase y, RSplit (GCaseClauses x) (GCaseClauses y)) =>
         GInstrCase (x :+: y) where
  type GCaseClauses (x :+: y) = GCaseClauses x ++ GCaseClauses y
  gInstrCase clauses =
    let (leftClauses, rightClauses) = rsplit clauses
        lbranch = gInstrCase @x leftClauses
        rbranch = gInstrCase @y rightClauses
    in IF_LEFT lbranch rbranch

instance GInstrCaseBranch ctor x => GInstrCase (G.C1 ('G.MetaCons ctor _1 _2) x) where
  type GCaseClauses (G.C1 ('G.MetaCons ctor _1 _2) x) = '[GCaseBranchInput ctor x]
  gInstrCase (clause :& RNil) = gInstrCaseBranch @ctor @x clause

-- | Traverse a single contructor and supply its field to instruction in case clause.
class GIsoValue x =>
      GInstrCaseBranch (ctor :: Symbol) (x :: Kind.Type -> Kind.Type) where

  type GCaseBranchInput ctor x :: CaseClauseParam

  gInstrCaseBranch
    :: CaseClause inp out (GCaseBranchInput ctor x)
    -> Instr (GValueType x ': inp) out

instance
  ( GIsoValue x, GIsoValue y
  , TypeError ('Text "Cannot pattern match on constructors with more than 1 field"
               ':$$: 'Text "Consider using tuples instead")
  ) => GInstrCaseBranch ctor (x :*: y) where

  type GCaseBranchInput ctor (x :*: y) = 'CaseClauseParam ctor 'NoFields
  gInstrCaseBranch = error "impossible"

instance GInstrCaseBranch ctor x => GInstrCaseBranch ctor (G.S1 i x) where
  type GCaseBranchInput ctor (G.S1 i x) = GCaseBranchInput ctor x
  gInstrCaseBranch = gInstrCaseBranch @ctor @x

instance IsoValue a => GInstrCaseBranch ctor (G.Rec0 a) where
  type GCaseBranchInput ctor (G.Rec0 a) = 'CaseClauseParam ctor ('OneField a)
  gInstrCaseBranch (CaseClause i) = i

instance GInstrCaseBranch ctor G.U1 where
  type GCaseBranchInput ctor G.U1 = 'CaseClauseParam ctor 'NoFields
  gInstrCaseBranch (CaseClause i) = DROP `Seq` i

-- Examples
----------------------------------------------------------------------------

_caseMyType :: Instr (ToT MyType ': s) (ToT Integer ': s)
_caseMyType = instrCase @MyType $
     #cMyCtor //-> Nop
  :& #cComplexThing //-> (DROP `Seq` PUSH (toVal @Integer 5))
  :& #cUselessThing //-> (DROP `Seq` PUSH (toVal @Integer 0))
  :& RNil

-- Another version, written via tuple.
_caseMyType2 :: Instr (ToT MyType ': s) (ToT Integer ': s)
_caseMyType2 = instrCase @MyType $ recFromTuple
  ( #cMyCtor //->
      Nop
  , #cComplexThing //->
      (DROP `Seq` PUSH (toVal @Integer 5))
  , #cUselessThing //->
      (DROP `Seq` PUSH (toVal @Integer 0))
  )

_caseMyEnum :: Instr (ToT MyEnum ': ToT Integer ': s) (ToT Integer ': s)
_caseMyEnum = instrCase @MyEnum $ recFromTuple
  ( #cCase1 //-> (DROP `Seq` PUSH (toVal @Integer 1))
  , #cCase2 //-> (DROP `Seq` PUSH (toVal @Integer 2))
  , #cCaseN //-> (DIP DROP `Seq` Nop)
  , #cCaseDef //-> Nop
  )
