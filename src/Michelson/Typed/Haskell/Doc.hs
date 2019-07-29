{-# LANGUAGE DerivingStrategies #-}

-- Dunno why it triggers
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Documentation of types appearing in contracts.
module Michelson.Typed.Haskell.Doc
  ( ADTRep
  , WithinParens (..)
  , TypeHasDoc (..)
  , TypeDocHaskellRep
  , TypeDocMichelsonRep
  , PolyTypeHasDocC
  , SomeTypeWithDoc (..)

  , HaveCommonTypeCtor
  , IsHomomorphic
  , genericTypeDocDependencies
  , customTypeDocMdReference
  , homomorphicTypeDocMdReference
  , poly1TypeDocMdReference
  , poly2TypeDocMdReference
  , homomorphicTypeDocHaskellRep
  , concreteTypeDocHaskellRep
  , haskellRepNoFields
  , haskellRepStripFieldPrefix
  , homomorphicTypeDocMichelsonRep
  , concreteTypeDocMichelsonRep

  , DType (..)
  , GTypeHasDoc
  , buildADTRep
  , applyWithinParens
  ) where

import Data.Char (isLower, isUpper, toLower)
import qualified Data.Kind as Kind
import Data.Singletons (SingI, demote)
import qualified Data.Text as T
import Fmt (Buildable, Builder, build, (+|), (|+))
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, TypeError, symbolVal)
import Named ((:!), (:?))
import qualified Text.Show
import Type.Showtype (Showtype(..))

import Michelson.Text
import Michelson.Typed.Aliases
import Michelson.Typed.Doc
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Print ()
import Michelson.Typed.T
import Tezos.Address
import Tezos.Core
import Tezos.Crypto
import Util.Generic
import Util.Markdown

-- | Stands for representation of some Haskell ADT corresponding to
-- Michelson value. Type parameter @a@ is what you put in place of
-- each field of the datatype, e.g. information about field type.
--
-- Outer list layer corresponds to union, and the inner one corresponds
-- to products within constructors. Constructors and fields names are present.
type ADTRep a = NonEmpty (Text, [(Maybe Text, a)])

-- | Show given 'ADTRep' in a neat way.
buildADTRep :: forall a. (WithinParens -> a -> Builder) -> ADTRep a -> Builder
buildADTRep buildField = \case
  p :| [] -> renderProduct (WithinParens False) (snd p)
  ps -> (mappend (mdItalic "one of" <> " \n")) $
        mconcat . map (("+ " <> ) . (<> "\n")) $
        map (renderNamedProduct (WithinParens True)) (toList ps)
  where
    renderNamedProduct :: WithinParens -> (Text, [(Maybe Text, a)]) -> Builder
    renderNamedProduct wp (ctor, tys) =
      mdBold (build ctor) <> " " <> renderProduct wp tys

    renderProduct :: WithinParens -> [(Maybe Text, a)] -> Builder
    renderProduct wp = \case
      [] -> "()"
      [t] -> renderNamedField wp t
      ts -> "(" <> (mconcat . intersperse ", " $ map (renderNamedField wp) ts) <> ")"

    renderNamedField :: WithinParens -> (Maybe Text, a) -> Builder
    renderNamedField wp (mfieldName, ty) =
      maybe "" buildFieldName mfieldName <>
      buildField wp ty

-- | Remove fields names from 'ADTRep'.
--
-- Sometimes there is no sense in showing them in the doc. Example: newtypes.
mapADTRepFields :: (Maybe Text -> Maybe Text) -> ADTRep a -> ADTRep a
mapADTRepFields f = fmap . second . fmap $ first f

-- | How field names should be displayed.
--
-- Result of this function call should appear right before rendered type
-- of that field.
buildFieldName :: Text -> Builder
buildFieldName name = mdItalic (mdBold (build name)) |+ " :"

-- | Whether given text should be rendered grouped in parentheses
-- (if they make sense).
newtype WithinParens = WithinParens Bool

applyWithinParens :: WithinParens -> Builder -> Builder
applyWithinParens (WithinParens wp) txt
  | wp = "(" <> txt <> ")"
  | otherwise = txt

-- | Description for a Haskell type appearing in documentation.
class TypeHasDoc a where
  -- | Name of type as it appears in definitions section.
  --
  -- Default definition derives name from Generics.
  -- If it does not fit, consider defining this function manually.
  -- (We tried using 'Data.Data' for this, but it produces names including
  -- module names which is not do we want).
  typeDocName :: Proxy a -> Text
  default typeDocName
    :: (Generic a, KnownSymbol (GenericTypeName a))
    => Proxy a -> Text
  typeDocName _ = toText $ symbolVal (Proxy @(GenericTypeName a))

  -- | Explanation of a type. Markdown formatting is allowed.
  typeDocMdDescription :: Builder

  -- | How reference to this type is rendered, in Markdown.
  --
  -- Examples:
  -- * @[Integer](#type-integer)@,
  -- * @[Maybe](#type-Maybe) [()](#type-unit)@.
  --
  -- Consider using one of the following functions as default implementation;
  -- which one to use depends on number of type arguments in your type:
  -- * 'homomorphicTypeDocMdReference'
  -- * 'poly1TypeDocMdReference'
  -- * 'poly2TypeDocMdReference'
  --
  -- If none of them fits your purposes precisely, consider using
  -- 'customTypeDocMdReference'.
  typeDocMdReference :: Proxy a -> WithinParens -> Builder
  default typeDocMdReference
    :: (Typeable a, IsHomomorphic a)
    => Proxy a -> WithinParens -> Builder
  typeDocMdReference = homomorphicTypeDocMdReference

  -- | All types which this type directly contains.
  --
  -- Used in automatic types discovery.
  typeDocDependencies :: Proxy a -> [SomeTypeWithDoc]
  default typeDocDependencies
    :: (Generic a, GTypeHasDoc (G.Rep a))
    => Proxy a -> [SomeTypeWithDoc]
  typeDocDependencies = genericTypeDocDependencies

  -- | For complex types - their immediate Haskell representation.
  --
  -- For primitive types set this to 'Nothing'.
  --
  -- For homomorphic types use 'homomorphicTypeDocHaskellRep' implementation.
  --
  -- For polymorhpic types consider using 'concreteTypeDocHaskellRep' as implementation.
  --
  -- Modifier 'haskellRepNoFields' can be used to hide names of fields,
  -- beneficial for newtypes.
  --
  -- Another modifier called 'haskellRepStripFieldPrefix' can be used for datatypes
  -- to leave only meaningful part of name in every field.
  typeDocHaskellRep :: TypeDocHaskellRep a
  default typeDocHaskellRep
    :: (Generic a, GTypeHasDoc (G.Rep a), IsHomomorphic a)
    => TypeDocHaskellRep a
  typeDocHaskellRep = haskellRepStripFieldPrefix homomorphicTypeDocHaskellRep

  -- | Final michelson representation of a type.
  --
  -- For homomorphic types use 'homomorphicTypeDocMichelsonRep' implementation.
  --
  -- For polymorhpic types consider using 'concreteTypeDocMichelsonRep' as implementation.
  typeDocMichelsonRep :: TypeDocMichelsonRep a
  default typeDocMichelsonRep
    :: (SingI (ToT a), IsHomomorphic a)
    => TypeDocMichelsonRep a
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

-- | Signature of 'typeDocHaskellRep' function.
--
-- When value is 'Just', it contains types which this type is built from.
--
-- First element of provided pair may contain name a concrete type which has
-- the same type constructor as @a@ (or just @a@ for homomorphic types), and
-- the second element of the pair - its unfolding in Haskell.
--
-- For example, for some @newtype MyNewtype = MyNewtype (Integer, Natural)@
-- we would not specify the first element in the pair because @MyNewtype@ is
-- already a concrete type, and second element would contain @(Integer, Natural)@.
-- For polymorhpic types like @newtype MyPolyNewtype a = MyPolyNewtype (Text, a)@,
-- we want to describe its representation on some example of @a@, because
-- working with type variables is too non-trivial; so the first element of
-- the pair may be e.g. @"MyPolyNewType Integer"@, and the second one shows
-- that it unfolds to @(Text, Integer)@.
--
-- When rendered, values of this type look like:
-- * @(Integer, Natural)@ - for homomorphic type.
-- * @MyError Integer = (Text, Integer)@ - concrete sample for polymorhpic type.
type TypeDocHaskellRep a =
  Proxy a -> Maybe (Maybe DocTypeRepLHS, ADTRep SomeTypeWithDoc)

-- | Signature of 'typeDocMichelsonRep' function.
--
-- As in 'TypeDocHaskellRep', set the first element of the pair to 'Nothing'
-- for primitive types, otherwise it stands as some instantiation of a type,
-- and its Michelson representation is given in the second element of the pair.
--
-- Examples of rendered representation:
-- * @pair int nat@ - for homomorphic type.
-- * @MyError Integer = pair string int@ - concrete sample for polymorhpic type.
type TypeDocMichelsonRep a =
  Proxy a -> (Maybe DocTypeRepLHS, T)

-- | Data hides some type implementing 'TypeHasDoc'.
data SomeTypeWithDoc where
  SomeTypeWithDoc :: TypeHasDoc td => Proxy td -> SomeTypeWithDoc

instance Show SomeTypeWithDoc where
  show (SomeTypeWithDoc di) = toString (typeDocName di)

-- | When rendering type's inner representation, this stands for name of
--
-- Having this makes sense for polymorhpic types, when you want to render
-- representation of some concrete instantiation of that type.
newtype DocTypeRepLHS = DocTypeRepLHS Text
  deriving newtype (IsString, Buildable)

-- | Doc element with description of a type.
data DType where
  DType :: TypeHasDoc a => Proxy a -> DType

instance DocItem DType where
  type DocItemPosition DType = 5000
  type DocItemPlacement DType = 'DocItemInDefinitions

  docItemSectionName = Just "Types"

  docItemRef (DType a) = DocItemRef $
    DocItemId ("types-" <> typeDocName a)

  docItemDependencies (DType (ap' :: Proxy a)) = do
    SomeTypeWithDoc tp <- typeDocDependencies ap'
    return $ SomeDocDefinitionItem (DType tp)

  docItemToMarkdown lvl (DType (ap' :: Proxy a)) =
    mconcat . catMaybes $
    [ Just mdSeparator
    , Just $ mdHeader lvl (mdTicked . build $ typeDocName ap')
    , Just $ typeDocMdDescription @a |+ "\n\n"
    , typeDocHaskellRep ap' <&> \(mlhs, rep) ->
        let
          buildField wp (SomeTypeWithDoc di) =
            typeDocMdReference di wp
          renderedRep =
             buildADTRep buildField rep
          rendered = case mlhs of
            Nothing ->
              mdSubsection "Structure" renderedRep
            Just lhs ->
              mdSubsection "Structure (example)" $
                mdTicked (build lhs) <> " = " <> renderedRep
        in rendered <> "\n\n"
    , Just $
        let (mlhs, rep) = typeDocMichelsonRep ap'
            renderedRep = mdTicked (build rep)
            rendered = case mlhs of
              Nothing -> mdSubsection "Final Michelson representation"
                         renderedRep
              Just lhs -> mdSubsection "Final Michelson representation (example)" $
                          mdTicked (build lhs) <> " = " <> renderedRep
        in rendered <> "\n\n"
    ]

-- Default implementations
----------------------------------------------------------------------------

-- | Require two types to be built from the same type constructor.
--
-- E.g. @HaveCommonTypeCtor (Maybe Integer) (Maybe Natural)@ is defined,
-- while @HaveCmmonTypeCtor (Maybe Integer) [Integer]@ is not.
class HaveCommonTypeCtor a b
instance HaveCommonTypeCtor ac bc => HaveCommonTypeCtor (ac a) (bc b)
instance HaveCommonTypeCtor a a

-- | Require this type to be homomorphic.
class IsHomomorphic a where
instance TypeError ('Text "Type is not homomorphic: " ':<>: 'ShowType (a b)) =>
         IsHomomorphic (a b)
instance {-# OVERLAPPABLE #-} IsHomomorphic a

-- | Render a reference to a type which consists of type constructor
-- (you have to provide name of this type constructor and documentation
-- for the whole type) and zero or more type arguments.
customTypeDocMdReference :: (Text, DType) -> [DType] -> WithinParens -> Builder
customTypeDocMdReference (typeCtorName, tyDoc) typeArgsDoc wp =
  let DocItemRef (DocItemId ctorDocItemId) = docItemRef tyDoc
  in applyWithinParens wpSmart $
     mconcat . intersperse " " $
      ( mdLocalRef (mdTicked $ build typeCtorName) ctorDocItemId
      : (typeArgsDoc <&> \(DType di) -> typeDocMdReference di (WithinParens True))
      )
    where
      -- If we are rendering an atomic thing, there is no need in parentheses
      -- around it
      wpSmart =
        let WithinParens wp' = wp
        in WithinParens (wp' && not (null typeArgsDoc))

-- | Derive 'typeDocMdReference', for homomorphic types only.
homomorphicTypeDocMdReference
  :: forall (t :: Kind.Type).
     (Typeable t, TypeHasDoc t, IsHomomorphic t)
  => Proxy t -> WithinParens -> Builder
homomorphicTypeDocMdReference tp _ =
  customTypeDocMdReference
    (toText $ showtype tp, DType tp)
    []
    (WithinParens False)

-- | Derive 'typeDocMdReference', for polymorphic type with one
-- type argument, like @Maybe Integer@.
poly1TypeDocMdReference
  :: forall t (r :: Kind.Type) (a :: Kind.Type).
      (r ~ t a, Typeable t, Each '[TypeHasDoc] [r, a], IsHomomorphic t)
  => Proxy r -> WithinParens -> Builder
poly1TypeDocMdReference tp =
  customTypeDocMdReference
    (toText $ showtype (Proxy @t), DType tp)
    [DType (Proxy @a)]

-- | Derive 'typeDocMdReference', for polymorphic type with two
-- type arguments, like @Lambda Integer Natural@.
poly2TypeDocMdReference
  :: forall t (r :: Kind.Type) (a :: Kind.Type) (b :: Kind.Type).
      (r ~ t a b, Typeable t, Each '[TypeHasDoc] [r, a, b], IsHomomorphic t)
  => Proxy r -> WithinParens -> Builder
poly2TypeDocMdReference tp =
  customTypeDocMdReference
    (toText $ showtype (Proxy @t), DType tp)
    [ DType (Proxy @a)
    , DType (Proxy @b)
    ]

-- | Implement 'typeDocDependencies' via getting all immediate fields
-- of a datatype.
--
-- Note: this will not include phantom types, I'm not sure yet how this
-- scenario should be handled (@martoon).
genericTypeDocDependencies
  :: forall a.
      (Generic a, GTypeHasDoc (G.Rep a))
  => Proxy a -> [SomeTypeWithDoc]
genericTypeDocDependencies _ = do
  (_, products) <- toList $ gTypeDocHaskellRep @(G.Rep a)
  (_, ty) <- products
  return ty

-- | Implement 'typeDocHaskellRep' for a homomorphic type.
--
-- Note that it does not require your type to be of 'IsHomomorphic' instance,
-- which can be useful for some polymorhpic types which, for documentation
-- purposes, we want to consider homomorphic.
-- Example: 'Operation' is in fact polymorhpic, but we don't want this fact to
-- be reflected in the documentation.
homomorphicTypeDocHaskellRep
  :: forall a.
     (Generic a, GTypeHasDoc (G.Rep a))
  => TypeDocHaskellRep a
homomorphicTypeDocHaskellRep _ = Just
  ( Nothing
  , gTypeDocHaskellRep @(G.Rep a)
  )

-- | Implement 'typeDocHaskellRep' on example of given concrete type.
--
-- This is a best effort attempt to implement 'typeDocHaskellRep' for polymorhpic
-- types, as soon as there is no simple way to preserve type variables when
-- automatically deriving Haskell representation of a type.
concreteTypeDocHaskellRep
  :: forall a b.
     ( Typeable a, IsoValue a, Generic a, GTypeHasDoc (G.Rep a)
     , HaveCommonTypeCtor b a
     )
  => TypeDocHaskellRep b
concreteTypeDocHaskellRep _ = Just
  ( Just (DocTypeRepLHS . toText . showtype $ Proxy @a)
  , gTypeDocHaskellRep @(G.Rep a)
  )

-- | Erase fields from Haskell datatype representation.
--
-- Use this when rendering fields names is undesired.
haskellRepNoFields :: TypeDocHaskellRep a -> TypeDocHaskellRep a
haskellRepNoFields mkRep =
  \p -> second (mapADTRepFields (const Nothing)) <$> mkRep p

-- | Cut fields prefixes which we use according to the style guide.
--
-- E.g. @cmMyField@ field will be transformed to @myField@.
haskellRepStripFieldPrefix
  :: HasCallStack
  => TypeDocHaskellRep a -> TypeDocHaskellRep a
haskellRepStripFieldPrefix mkRep =
  \p -> second (mapADTRepFields (fmap stripPrefix)) <$> mkRep p
  where
    stripPrefix fieldName =
      case T.uncons $ T.dropWhile isLower fieldName of
        Nothing -> error $ "Field '" <> fieldName <> "' has no prefix"
        Just (c, cs) ->
          -- For fields like @ciUSPosition@ we should not lead the first letter
          -- to lower case like @uSPosition@.
          let isAbbreviation = case T.uncons cs of
                Just (c2, _)
                  | isUpper c2 -> True
                  | otherwise -> False
                Nothing -> False
          in T.cons (if isAbbreviation then c else toLower c) cs

-- | Implement 'typeDocMichelsonRep' for homomorphic type.
homomorphicTypeDocMichelsonRep
  :: forall a.
     SingI (ToT a)
  => TypeDocMichelsonRep a
homomorphicTypeDocMichelsonRep _ =
  ( Nothing
  , demote @(ToT a)
  )

-- | Implement 'typeDocMichelsonRep' on example of given concrete type.
--
-- This function exists for the same reason as 'concreteTypeDocHaskellRep'.
concreteTypeDocMichelsonRep
  :: forall a b.
     (Typeable a, SingI (ToT a), HaveCommonTypeCtor b a)
  => TypeDocMichelsonRep b
concreteTypeDocMichelsonRep _ =
  ( Just (DocTypeRepLHS . toText . showtype $ Proxy @a)
  , demote @(ToT a)
  )

-- | Generic traversal for automatic deriving of some methods in 'TypeHasDoc'.
class GTypeHasDoc (x :: Kind.Type -> Kind.Type) where
  gTypeDocHaskellRep :: ADTRep SomeTypeWithDoc

instance GTypeHasDoc x => GTypeHasDoc (G.D1 i x) where
  gTypeDocHaskellRep = gTypeDocHaskellRep @x

instance (GTypeHasDoc x, GTypeHasDoc y) => GTypeHasDoc (x :+: y) where
  gTypeDocHaskellRep = gTypeDocHaskellRep @x <> gTypeDocHaskellRep @y

instance (GProductHasDoc x, KnownSymbol ctor) =>
         GTypeHasDoc (G.C1 ('G.MetaCons ctor _1 _2) x) where
  gTypeDocHaskellRep = one $
    ( toText $ symbolVal (Proxy @ctor)
    , gProductDocHaskellRep @x
    )

instance TypeError ('Text "Cannot derive documentation for void-like type") =>
    GTypeHasDoc G.V1 where
  gTypeDocHaskellRep = error "impossible"

-- | Product type traversal for 'TypeHasDoc'.
class GProductHasDoc (x :: Kind.Type -> Kind.Type) where
  gProductDocHaskellRep :: [(Maybe Text, SomeTypeWithDoc)]

instance (GProductHasDoc x, GProductHasDoc y) => GProductHasDoc (x :*: y) where
  gProductDocHaskellRep = gProductDocHaskellRep @x <> gProductDocHaskellRep @y

instance TypeHasDoc a =>
         GProductHasDoc (G.S1 ('G.MetaSel 'Nothing _1 _2 _3) (G.Rec0 a)) where
  gProductDocHaskellRep = one (Nothing, SomeTypeWithDoc (Proxy @a))

instance (TypeHasDoc a, KnownSymbol field) =>
         GProductHasDoc (G.S1 ('G.MetaSel ('Just field) _1 _2 _3) (G.Rec0 a)) where
  gProductDocHaskellRep = one
    ( Just $ toText (symbolVal $ Proxy @field)
    , SomeTypeWithDoc (Proxy @a)
    )

instance GProductHasDoc G.U1 where
  gProductDocHaskellRep = mempty

-- Instances
----------------------------------------------------------------------------

-- | Constraint, required when deriving 'TypeHasDoc' for polymorphic type
-- with the least possible number of methods defined manually.
type PolyTypeHasDocC ts = Each '[TypeHasDoc] ts

-- | Version of 'PolyTypeHasDocC' for comparable types.
type PolyCTypeHasDocC ts = Each '[TypeHasDoc] ts

instance TypeHasDoc Integer where
  typeDocName _ = "Integer"
  typeDocMdDescription = "Signed number."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc Natural where
  typeDocName _ = "Natural"
  typeDocMdDescription = "Unsigned number."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc MText where
  typeDocName _ = "Text"
  typeDocMdReference p = customTypeDocMdReference ("Text", DType p) []
  -- TODO [TM-224]: mention tutorial section explaining string constraints.
  typeDocMdDescription = "Michelson string."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc Bool where
  typeDocName _ = "Bool"
  typeDocMdDescription = "Bool primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc ByteString where
  typeDocName _ = "ByteString"
  typeDocMdDescription = "Bytes primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc Mutez where
  typeDocName _ = "Mutez"
  typeDocMdDescription = "Mutez primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc KeyHash where
  typeDocName _ = "KeyHash"
  typeDocMdDescription = "KeyHash primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc Timestamp where
  typeDocName _ = "Timestamp"
  typeDocMdDescription = "Timestamp primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc Address where
  typeDocName _ = "Address"
  typeDocMdDescription = "Address primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc PublicKey where
  typeDocName _ = "PublicKey"
  typeDocMdDescription = "PublicKey primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc Signature where
  typeDocName _ = "Signature"
  typeDocMdDescription = "Signature primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing

instance TypeHasDoc () where
  typeDocName _ = "()"
  typeDocMdDescription = "Unit primitive."
  typeDocDependencies _ = []

instance PolyTypeHasDocC '[a] => TypeHasDoc [a] where
  typeDocName _ = "List"
  typeDocMdDescription = "List primitive."
  typeDocMdReference _ =
    -- poly1TypeDocMdReference would produce text like @[] Integer@, we want
    -- to replace this @[]@ with @List@.
    customTypeDocMdReference ("List", DType (Proxy @[a])) [DType (Proxy @a)]
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @[Integer]

instance PolyTypeHasDocC '[a] => TypeHasDoc (Maybe a) where
  typeDocMdDescription = "Option primitive."
  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Maybe Integer)

instance PolyTypeHasDocC [l, r] => TypeHasDoc (Either l r) where
  typeDocMdDescription = "Or primitive."
  typeDocMdReference = poly2TypeDocMdReference
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Either Integer Natural)

instance PolyTypeHasDocC [a, b] => TypeHasDoc (a, b) where
  typeDocName _ = "(a, b)"
  typeDocMdReference _ = tupleTypeDocReference
    [ typeDocMdReference (Proxy @a) (WithinParens False)
    , typeDocMdReference (Proxy @b) (WithinParens False)
    ]
  typeDocMdDescription = "Pair primitive."
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Integer, Natural)

instance PolyCTypeHasDocC '[a] => TypeHasDoc (Set a) where
  typeDocName _ = "Set"
  typeDocMdReference = poly1TypeDocMdReference
  typeDocMdDescription = "Set primitive."
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @a)]
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Set Integer)

instance TypeHasDoc Operation where
  typeDocName _ = "Operation"
  typeDocMdReference tp = customTypeDocMdReference ("Operation", DType tp) []
  typeDocMdDescription = "Operation primitive."
  typeDocDependencies _ = []
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance PolyTypeHasDocC '[cp] => TypeHasDoc (ContractAddr cp) where
  typeDocName _ = "Contract"
  typeDocMdReference = poly1TypeDocMdReference
  typeDocMdDescription = "Contract primitive with given type of parameter."
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @cp)]
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(ContractAddr Integer)

instance (PolyCTypeHasDocC '[k], PolyTypeHasDocC '[v], Ord k) =>
         TypeHasDoc (Map k v) where
  typeDocName _ = "Map"
  typeDocMdReference = poly2TypeDocMdReference
  typeDocMdDescription = "Map primitive."
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @k), SomeTypeWithDoc (Proxy @v)]
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Map Integer Natural)

instance (PolyCTypeHasDocC '[k], PolyTypeHasDocC '[v], Ord k) =>
         TypeHasDoc (BigMap k v) where
  typeDocName _ = "BigMap"
  typeDocMdReference = poly2TypeDocMdReference
  typeDocMdDescription = "BigMap primitive."
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @k), SomeTypeWithDoc (Proxy @v)]
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(BigMap Integer Natural)


tupleTypeDocReference :: [Builder] -> WithinParens -> Builder
tupleTypeDocReference vs _ = "(" +| mconcat (intersperse ", " $ map build vs) |+ ")"

instance PolyTypeHasDocC [a, b, c] => TypeHasDoc (a, b, c) where
  typeDocName _ = "(a, b, c)"
  typeDocMdReference _ = tupleTypeDocReference
    [ typeDocMdReference (Proxy @a) (WithinParens False)
    , typeDocMdReference (Proxy @b) (WithinParens False)
    , typeDocMdReference (Proxy @c) (WithinParens False)
    ]
  typeDocMdDescription = "Tuple of size 3."
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(Integer, Natural, MText)

instance PolyTypeHasDocC [a, b, c, d] => TypeHasDoc (a, b, c, d) where
  typeDocName _ = "(a, b, c, d)"
  typeDocMdReference _ = tupleTypeDocReference
    [ typeDocMdReference (Proxy @a) (WithinParens False)
    , typeDocMdReference (Proxy @b) (WithinParens False)
    , typeDocMdReference (Proxy @c) (WithinParens False)
    , typeDocMdReference (Proxy @d) (WithinParens False)
    ]
  typeDocMdDescription = "Tuple of size 4."
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep =
    -- Starting from tuple of size 4 the exact types should not matter to a reader,
    -- rather the resulting pairs tree.
    concreteTypeDocMichelsonRep @((), (), (), ())

instance PolyTypeHasDocC [a, b, c, d, e] => TypeHasDoc (a, b, c, d, e) where
  typeDocName _ = "(a, b, c, d, e)"
  typeDocMdDescription = "Tuple of size 5."
  typeDocMdReference _ = tupleTypeDocReference
    [ typeDocMdReference (Proxy @a) (WithinParens False)
    , typeDocMdReference (Proxy @b) (WithinParens False)
    , typeDocMdReference (Proxy @c) (WithinParens False)
    , typeDocMdReference (Proxy @d) (WithinParens False)
    , typeDocMdReference (Proxy @e) (WithinParens False)
    ]
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep =
    concreteTypeDocMichelsonRep @((), (), (), (), ())

instance PolyTypeHasDocC [a, b, c, d, e, f] => TypeHasDoc (a, b, c, d, e, f) where
  typeDocName _ = "(a, b, c, d, e, f)"
  typeDocMdReference _ = tupleTypeDocReference
    [ typeDocMdReference (Proxy @a) (WithinParens False)
    , typeDocMdReference (Proxy @b) (WithinParens False)
    , typeDocMdReference (Proxy @c) (WithinParens False)
    , typeDocMdReference (Proxy @d) (WithinParens False)
    , typeDocMdReference (Proxy @e) (WithinParens False)
    , typeDocMdReference (Proxy @f) (WithinParens False)
    ]
  typeDocMdDescription = "Tuple of size 6."
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep =
    concreteTypeDocMichelsonRep @((), (), (), (), (), ())

instance PolyTypeHasDocC [a, b, c, d, e, f, g] => TypeHasDoc (a, b, c, d, e, f, g) where
  typeDocName _ = "(a, b, c, d, e, f, g)"
  typeDocMdReference _ = tupleTypeDocReference
    [ typeDocMdReference (Proxy @a) (WithinParens False)
    , typeDocMdReference (Proxy @b) (WithinParens False)
    , typeDocMdReference (Proxy @c) (WithinParens False)
    , typeDocMdReference (Proxy @d) (WithinParens False)
    , typeDocMdReference (Proxy @e) (WithinParens False)
    , typeDocMdReference (Proxy @f) (WithinParens False)
    , typeDocMdReference (Proxy @g) (WithinParens False)
    ]
  typeDocMdDescription = "Tuple of size 7."
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep =
    concreteTypeDocMichelsonRep @((), (), (), (), (), (), ())


instance (TypeHasDoc a, KnownSymbol n) => TypeHasDoc (n :! a) where
  typeDocName _ = typeDocName (Proxy @a)
  typeDocMdReference _ wp =
    applyWithinParens wp $
    buildFieldName (toText (symbolVal $ Proxy @n)) +| " " +|
    typeDocMdReference (Proxy @a) (WithinParens False) |+ ""
  typeDocMdDescription = typeDocMdDescription @a
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @a)]
  typeDocHaskellRep _ = typeDocHaskellRep (Proxy @a)
  typeDocMichelsonRep _ = typeDocMichelsonRep (Proxy @a)

instance (Typeable a, TypeHasDoc (Maybe a), KnownSymbol n) => TypeHasDoc (n :? a) where
  typeDocName _ = typeDocName (Proxy @(Maybe a))
  typeDocMdReference _ wp =
    applyWithinParens wp $
    buildFieldName (toText (symbolVal $ Proxy @n)) +| " " +|
    typeDocMdReference (Proxy @(Maybe a)) (WithinParens False) |+ ""
  typeDocMdDescription = typeDocMdDescription @(Maybe a)
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @(Maybe a))]
  typeDocHaskellRep _ = typeDocHaskellRep (Proxy @(Maybe a))
  typeDocMichelsonRep _ = typeDocMichelsonRep (Proxy @(Maybe a))
