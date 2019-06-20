{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Instructions working on sum types derived from Haskell ones.
module Michelson.Typed.Haskell.Instr.Sum
  ( InstrWrapC
  , InstrCaseC
  , InstrUnwrapC
  , instrWrap
  , hsWrap
  , instrCase
  , (//->)
  , instrUnwrapUnsafe
  , hsUnwrap

  , CaseClauseParam (..)
  , CaseClause (..)
  , CaseClauses

  , Branch (..)
  , Path
  , CtorField (..)
  , ExtractCtorField
  , AppendCtorField
  , AppendCtorFieldAxiom
  , appendCtorFieldAxiom
  , GetCtorField
  , CtorHasOnlyField
  , CtorOnlyField
  , MyCompoundType
  ) where

import Data.Constraint (Dict(..))
import qualified Data.Kind as Kind
import Data.Singletons (SingI(..))
import Data.Type.Bool (If, type (||))
import Data.Type.Equality (type (==))
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Derived (Label)
import Data.Vinyl.TypeLevel (type (++))
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (AppendSymbol, ErrorMessage(..), Symbol, TypeError)
import Named ((:!))
import Type.Reflection ((:~:)(Refl))
import Unsafe.Coerce (unsafeCoerce)

import Michelson.Typed.Haskell.Instr.Helpers
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Instr
import Michelson.Typed.T
import Michelson.Text (mt)
import Michelson.Typed.Value
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)
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

-- | Get /something/ as field of the given constructor.
type family ExtractCtorField (cf :: CtorField) where
  ExtractCtorField ('OneField t) = t
  ExtractCtorField 'NoFields = ()

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
  -- Alternative approach I compare this unsafe one with
  -- would be to implement @instance SingI@ for 'CtorField' and consider cases
  -- of 'CtorField' one by one, but this would require propagading
  -- @SingI (cf :: CtorField)@ constraint to all users of 'appendCtorFieldAxiom'.
  unsafeCoerce $ Dict @(AppendCtorFieldAxiom 'NoFields '[])

-- | Result of constructor lookup - entry type and path to it in the tree.
data LookupNamedResult = LNR CtorField Path

type family LnrFieldType (lnr :: LookupNamedResult) where
  LnrFieldType ('LNR f _) = f
type family LnrBranch (lnr :: LookupNamedResult) :: Path where
  LnrBranch ('LNR _ p) = p

-- | Transitively find constructor of some sum type by its name.
--
-- Transitivity means that if sum type consists of other sum types,
-- they will be searched recursively.
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
     'Text " has no (sub)constructor " ':<>: 'ShowType name)

type family GLookupNamed (name :: Symbol) (x :: Kind.Type -> Kind.Type)
          :: Maybe LookupNamedResult where
  GLookupNamed name (G.D1 _ x) = GLookupNamed name x
  GLookupNamed name (G.C1 ('G.MetaCons ctorName _ _) x) =
    -- This case corresponds to going straight into a data type (as
    -- opposed to going left or right when we encounter a sum type),
    -- so we need to prepend 'S' here if we successfully find a
    -- constructor.
    PrependS
    -- If we encounter a constructor with requested name, we consider
    -- search successful.
    (If (name == ctorName || name == ("c" `AppendSymbol` ctorName))
       ('Just $ 'LNR (GExtractFields x) '[])
        -- If we do not, we check whether we can go deeper (see
        -- description below).
        (If (GCanGoDeeper x)
        -- And here we essentially just recursively call this search.
        (GLookupNamedDeeper name x)
        -- Or return 'Nothing' if we can not go deeper.
         'Nothing
      )
    )
  GLookupNamed name (x :+: y) =
    LNMergeFound name (GLookupNamed name x) (GLookupNamed name y)
  GLookupNamed _ G.V1 = 'Nothing

-- Prepend 'S' to the path inside 'LookupNamedResult' (if 'Just' is passed).
-- Should be done in the case when we go straight into a data type.
type family PrependS (x :: Maybe LookupNamedResult) :: Maybe LookupNamedResult where
  PrependS 'Nothing = 'Nothing
  PrependS ('Just ('LNR cf path)) = 'Just ('LNR cf ('S ': path))

-- Helper type family to check whether we should search for a
-- constructor inside given data type which is supposed to be part of
-- another constructor. Basically we can go deeper only if we
-- encounter another ADT with a constructor storing at least something
-- and which is not a primitive type. We do not go deeper when we
-- encounter a product type here, because it means that there are multiple
-- fields inside one constructor (it is not supported).
type family GCanGoDeeper (x :: Kind.Type -> Kind.Type) :: Bool where
  GCanGoDeeper (_ :+: _) = 'True
  GCanGoDeeper (G.D1 _ x) = GCanGoDeeper x
  GCanGoDeeper (G.S1 _ (G.Rec0 x)) = CanGoDeeper x
  GCanGoDeeper (G.C1 _ _) = 'True
  GCanGoDeeper G.V1 = 'False
  GCanGoDeeper G.U1 = 'False
  GCanGoDeeper (_ :*: _) = 'False
  GCanGoDeeper x = TypeError
    ('Text "GCanGoDeeper encountered unexpected pattern: " ':<>: 'ShowType x)

-- Some types don't have Generic instances (e. g. primitives like Integer), so
-- we need another type family to handle them.
type family CanGoDeeper (x :: Kind.Type) :: Bool where
  CanGoDeeper Integer = 'False
  CanGoDeeper Natural = 'False
  CanGoDeeper Text = 'False
  CanGoDeeper Bool = 'False
  CanGoDeeper ByteString = 'False
  CanGoDeeper Mutez = 'False
  CanGoDeeper Address = 'False
  CanGoDeeper KeyHash = 'False
  CanGoDeeper Timestamp = 'False
  CanGoDeeper PublicKey = 'False
  CanGoDeeper Signature = 'False
  CanGoDeeper (ContractAddr _) = 'False
  CanGoDeeper (BigMap _ _) = 'False
  CanGoDeeper (Map _ _) = 'False
  CanGoDeeper (Set _) = 'False
  CanGoDeeper ([_]) = 'False
  CanGoDeeper x = GCanGoDeeper (G.Rep x)

type family GLookupNamedDeeper (name :: Symbol) (x :: Kind.Type -> Kind.Type)
          :: Maybe LookupNamedResult where
  GLookupNamedDeeper name (G.S1 _ (G.Rec0 y))  = GLookupNamed name (G.Rep y)
  GLookupNamedDeeper name _ = TypeError
    ('Text "Attempt to go deeper failed while looking for sum type constructor"
     ':<>: 'ShowType name
     ':$$:
     'Text "Make sure that all constructors have exactly one field inside."
    )

type family LNMergeFound
  (name :: Symbol)
  (f1 :: Maybe LookupNamedResult)
  (f2 :: Maybe LookupNamedResult)
    :: Maybe LookupNamedResult where
  LNMergeFound _ 'Nothing 'Nothing = 'Nothing
  LNMergeFound _ ('Just ('LNR a p)) 'Nothing = 'Just $ 'LNR a ('L ': p)
  LNMergeFound _ 'Nothing ('Just ('LNR a p)) = 'Just $ 'LNR a ('R ': p)
  LNMergeFound name ('Just _) ('Just _) = TypeError
    ('Text "Ambiguous reference to datatype constructor: " ':<>: 'ShowType name)

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

-- | Get type of constructor fields (one or zero) referred by given datatype
-- and name.
type GetCtorField dt ctor =
  LnrFieldType (GetNamed ctor dt)

-- | Expect referred constructor to have only one field (in form of constraint)
-- and extract its type.
type CtorHasOnlyField ctor dt f =
  GetCtorField dt ctor ~ 'OneField f

type family RequireOneField (ctor :: Symbol) (cf :: CtorField) :: Kind.Type where
  RequireOneField _ ('OneField f) = f
  RequireOneField ctor 'NoFields = TypeError
    ('Text "Expected exactly one field" ':$$:
     'Text "In constructor " ':<>: 'ShowType ctor)

-- | Expect referred constructor to have only one field
-- (otherwise compile error is raised) and extract its type.
type CtorOnlyField name dt =
  RequireOneField name (GetCtorField dt name)

----------------------------------------------------------------------------
-- Constructor wrapping instruction
----------------------------------------------------------------------------

-- | Wrap given element into a constructor with the given name.
--
-- Mentioned constructor must have only one field.
--
-- Since labels interpretable by "OverloadedLabels" extension cannot
-- start with capital latter, prepend constructor name with letter
-- "c" (see examples below).
instrWrap
  :: forall dt name st.
     InstrWrapC dt name
  => Label name
  -> Instr (AppendCtorField (GetCtorField dt name) st)
           (ToT dt ': st)
instrWrap _ =
  gInstrWrap @(G.Rep dt) @(LnrBranch (GetNamed name dt))
                         @(LnrFieldType (GetNamed name dt))

type InstrWrapC dt name =
  ( IsoValue dt, Generic dt
  , GInstrWrap (G.Rep dt)
      (LnrBranch (GetNamed name dt))
      (LnrFieldType (GetNamed name dt))
  , GValueType (G.Rep dt) ~ ToT dt
  )

-- | Wrap a haskell value into a constructor with the given name.
--
-- This is symmetric to 'instrWrap'.
hsWrap
  :: forall dt name.
     InstrWrapC dt name
  => Label name
  -> ExtractCtorField (GetCtorField dt name)
  -> dt
hsWrap _ =
  G.to . gHsWrap @(G.Rep dt) @(LnrBranch (GetNamed name dt))
                             @(LnrFieldType (GetNamed name dt))

-- | Generic traversal for 'instrWrap'.
class GIsoValue x =>
  GInstrWrap
    (x :: Kind.Type -> Kind.Type)
    (path :: Path)
    (entryTy :: CtorField) where
  gInstrWrap
    :: GIsoValue x
    => Instr (AppendCtorField entryTy s) (GValueType x ': s)
  gHsWrap
    :: GIsoValue x
    => ExtractCtorField entryTy -> x p

instance GInstrWrap x path e => GInstrWrap (G.D1 i x) path e where
  gInstrWrap = gInstrWrap @x @path @e
  gHsWrap = G.M1 . gHsWrap @x @path @e

instance (GInstrWrap x path e, GIsoValue y, SingI (GValueType y)) =>
         GInstrWrap (x :+: y) ('L ': path) e where
  gInstrWrap = gInstrWrap @x @path @e `Seq` LEFT
  gHsWrap = G.L1 . gHsWrap @x @path @e

instance (GInstrWrap y path e, GIsoValue x, SingI (GValueType x)) =>
         GInstrWrap (x :+: y) ('R ': path) e where
  gInstrWrap = gInstrWrap @y @path @e `Seq` RIGHT
  gHsWrap = G.R1 . gHsWrap @y @path @e

instance (IsoValue e) =>
         GInstrWrap (G.C1 c (G.S1 i (G.Rec0 e))) '[ 'S] ('OneField e) where
  gInstrWrap = Nop
  gHsWrap = G.M1 . G.M1 . G.K1

-- This is the case when a sum type is part of another sum type.
-- Here 'sub' is intermediate sum type.
instance ( path ~ (x ': xs)
         , GInstrWrap (G.Rep sub) path e
         , Generic sub
         , GIsoValue (G.Rep sub), IsoValue sub, GValueType (G.Rep sub) ~ ToT sub
         ) =>
         GInstrWrap (G.C1 c (G.S1 i (G.Rec0 sub))) ('S ': x ': xs) e where
  gInstrWrap = gInstrWrap @(G.Rep sub) @path @e
  gHsWrap = G.M1 . G.M1 . G.K1 . G.to . gHsWrap @(G.Rep sub) @path @e

instance GInstrWrap (G.C1 c G.U1) '[ 'S] 'NoFields where
  gInstrWrap = PUSH VUnit
  gHsWrap () = G.M1 G.U1

-- Examples
----------------------------------------------------------------------------

-- TODO [TM-186] Consider moving this stuff to test-suite using
-- doctest. Or at least add tests for correctness.

data MyType
  = MyCtor Integer
  | ComplexThing ()
  | UselessThing ("field1" :! ByteString, "field2" :! ())
  deriving stock Generic
  deriving anyclass IsoValue

_MyTypeMyCtor ::
  GetNamed "cMyCtor" MyType :~: 'LNR ('OneField Integer) '[ 'L, 'S]
_MyTypeMyCtor = Refl

_MyTypeComplexThing ::
  GetNamed "cComplexThing" MyType :~: 'LNR ('OneField ()) '[ 'R, 'L, 'S]
_MyTypeComplexThing = Refl

_wrapMyType1 :: Instr (ToT Integer ': s) (ToT MyType ': s)
_wrapMyType1 = instrWrap @MyType #cMyCtor

-- If "c" prefix is too annoying, one can construct labels without
-- OverloadedLabels extension.
_wrapMyType1' :: Instr (ToT Integer ': s) (ToT MyType ': s)
_wrapMyType1' = instrWrap @MyType @"MyCtor" fromLabel

_wrapMyType2 :: Instr (ToT () ': s) (ToT MyType ': s)
_wrapMyType2 = instrWrap @MyType #cComplexThing

_wrapMyType3 :: Instr (ToT (ByteString, ()) ': s) (ToT MyType ': s)
_wrapMyType3 = instrWrap @MyType #cUselessThing

data MyType' = WrapBool Bool
  deriving stock Generic
  deriving anyclass IsoValue

_MyType'WrapBool ::
  GetNamed "cWrapBool" MyType' :~: 'LNR ('OneField Bool) '[ 'S]
_MyType'WrapBool = Refl

_wrapMyType' :: Instr (ToT Bool ': s) (ToT MyType' ': s)
_wrapMyType' = instrWrap @MyType' #cWrapBool

data MyCompoundType
  = CompoundPart1 MyType
  | CompoundPart2 Integer
  | CompoundPart3 Address
  | CompoundPart4 MyType'
  deriving stock Generic
  deriving anyclass IsoValue

_MyCompoundTypeCompoundPart1 ::
  GetNamed "cCompoundPart1" MyCompoundType :~: 'LNR ('OneField MyType) '[ 'L, 'L, 'S]
_MyCompoundTypeCompoundPart1 = Refl

_MyCompoundTypeMyCtor ::
  GetNamed "cMyCtor" MyCompoundType :~: 'LNR ('OneField Integer) '[ 'L, 'L, 'S, 'L, 'S]
_MyCompoundTypeMyCtor = Refl

_MyCompoundTypeCompoundPart2 ::
  GetNamed "cCompoundPart2" MyCompoundType :~: 'LNR ('OneField Integer) '[ 'L, 'R, 'S]
_MyCompoundTypeCompoundPart2 = Refl

_MyCompoundTypeCompoundPart4 ::
  GetNamed "cCompoundPart4" MyCompoundType :~: 'LNR ('OneField MyType') '[ 'R, 'R, 'S]
_MyCompoundTypeCompoundPart4 = Refl

_MyCompoundTypeWrapBool ::
  GetNamed "cWrapBool" MyCompoundType :~: 'LNR ('OneField Bool) '[ 'R, 'R, 'S, 'S]
_MyCompoundTypeWrapBool = Refl

_wrapMyCompoundType1 :: Instr (ToT Integer ': s) (ToT MyCompoundType ': s)
_wrapMyCompoundType1 = instrWrap @MyCompoundType #cMyCtor

_wrapMyCompoundType2 :: Instr (ToT Integer ': s) (ToT MyCompoundType ': s)
_wrapMyCompoundType2 = instrWrap @MyCompoundType #cCompoundPart2

_wrapMyCompoundType3 :: Instr (ToT Bool ': s) (ToT MyCompoundType ': s)
_wrapMyCompoundType3 = instrWrap @MyCompoundType #cWrapBool

_wrapMyCompoundType4 :: Instr (ToT MyType' ': s) (ToT MyCompoundType ': s)
_wrapMyCompoundType4 = instrWrap @MyCompoundType #cCompoundPart4

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

----------------------------------------------------------------------------
-- Constructor unwrapping instruction
----------------------------------------------------------------------------

-- | Unwrap a constructor with the given name.
--
-- Rules which apply to 'instrWrap' function work here as well.
-- Although, unlike @instrWrap@, this function does not work for nullary
-- constructors.
instrUnwrapUnsafe
  :: forall dt name st.
     InstrUnwrapC dt name
  => Label name
  -> Instr (ToT dt ': st)
           (ToT (CtorOnlyField name dt) ': st)
instrUnwrapUnsafe _ =
  gInstrUnwrapUnsafe @(G.Rep dt) @(LnrBranch (GetNamed name dt))
                                 @(CtorOnlyField name dt)

type InstrUnwrapC dt name =
  ( IsoValue dt, Generic dt
  , GInstrUnwrap (G.Rep dt)
      (LnrBranch (GetNamed name dt))
      (CtorOnlyField name dt)
  , GValueType (G.Rep dt) ~ ToT dt
  )

-- | Try to unwrap a constructor with the given name.
hsUnwrap
  :: forall dt name.
     InstrUnwrapC dt name
  => Label name
  -> dt
  -> Maybe (CtorOnlyField name dt)
hsUnwrap _ =
  gHsUnwrap @(G.Rep dt) @(LnrBranch (GetNamed name dt))
            @(CtorOnlyField name dt) .
  G.from

class GIsoValue x =>
  GInstrUnwrap
    (x :: Kind.Type -> Kind.Type)
    (path :: Path)
    (entryTy :: Kind.Type) where
  gInstrUnwrapUnsafe
    :: GIsoValue x
    => Instr (GValueType x ': s) (ToT entryTy ': s)
  gHsUnwrap
    :: x p -> Maybe entryTy

instance GInstrUnwrap x path e => GInstrUnwrap (G.D1 i x) path e where
  gInstrUnwrapUnsafe = gInstrUnwrapUnsafe @x @path @e
  gHsUnwrap = gHsUnwrap @x @path @e . G.unM1

instance (GInstrUnwrap x path e, GIsoValue y, SingI (GValueType y)) =>
         GInstrUnwrap (x :+: y) ('L ': path) e where
  gInstrUnwrapUnsafe = IF_LEFT (gInstrUnwrapUnsafe @x @path @e) failWithWrongCtor
  gHsUnwrap = \case
    G.L1 x -> gHsUnwrap @x @path @e x
    G.R1 _ -> Nothing

instance (GInstrUnwrap y path e, GIsoValue x, SingI (GValueType x)) =>
         GInstrUnwrap (x :+: y) ('R ': path) e where
  gInstrUnwrapUnsafe = IF_LEFT failWithWrongCtor (gInstrUnwrapUnsafe @y @path @e)
  gHsUnwrap = \case
    G.R1 y -> gHsUnwrap @y @path @e y
    G.L1 _ -> Nothing

instance (IsoValue e) =>
         GInstrUnwrap (G.C1 c (G.S1 i (G.Rec0 e))) '[ 'S] e where
  gInstrUnwrapUnsafe = Nop
  gHsUnwrap = Just . G.unK1 . G.unM1 . G.unM1

-- This is the case when a sum type is part of another sum type.
-- Here 'sub' is intermediate sum type.
instance ( path ~ (x ': xs)
         , GInstrUnwrap (G.Rep sub) path e
         , Generic sub
         , GIsoValue (G.Rep sub), IsoValue sub, GValueType (G.Rep sub) ~ ToT sub
         ) =>
         GInstrUnwrap (G.C1 c (G.S1 i (G.Rec0 sub))) ('S ': x ': xs) e where
  gInstrUnwrapUnsafe = gInstrUnwrapUnsafe @(G.Rep sub) @path @e
  gHsUnwrap = gHsUnwrap @(G.Rep sub) @path @e . G.from . G.unK1 . G.unM1 . G.unM1

-- | Failure indicating that we expected value created with one constructor,
-- but got value with different one.
failWithWrongCtor :: Instr i o
failWithWrongCtor =
  PUSH (toVal [mt|unwrapUnsafe: unexpected constructor|]) `Seq`
  FAILWITH

-- Examples
----------------------------------------------------------------------------

_unwrapMyType :: Instr (ToT MyType ': s) (ToT Integer ': s)
_unwrapMyType = instrUnwrapUnsafe @MyType #cMyCtor

_unwrapMyCompoundType :: Instr (ToT MyCompoundType ': s) (ToT Integer ': s)
_unwrapMyCompoundType = instrUnwrapUnsafe @MyCompoundType #cMyCtor

_unwrapMyCompoundType2 :: Instr (ToT MyCompoundType ': s) (ToT Address ': s)
_unwrapMyCompoundType2 = instrUnwrapUnsafe @MyCompoundType #cCompoundPart3

_unwrapMyCompoundType3 :: Instr (ToT MyCompoundType ': s) (ToT Bool ': s)
_unwrapMyCompoundType3 = instrUnwrapUnsafe @MyCompoundType #cWrapBool

_unwrapMyCompoundType4 :: Instr (ToT MyCompoundType ': s) (ToT MyType' ': s)
_unwrapMyCompoundType4 = instrUnwrapUnsafe @MyCompoundType #cCompoundPart4
