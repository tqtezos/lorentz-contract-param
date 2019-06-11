{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Representation of Haskell sum types via loosy typed Michelson values,
-- useful for e.g. errors and enums.
--
-- In particular, ADT sum can be represented as constructor name + data
-- it carries. Such expression does not have particular type because
-- different constructors may carry different data, and we avoid lifting
-- this data to a union in order to keep only the significant parts
-- (and thus not to confuse the client).
module Michelson.Typed.Haskell.LooseSum
  ( ComposeResult (..)
  , fromTaggedVal
  , toTaggedVal
  , LooseSumC
  ) where

import qualified Data.Kind as Kind
import Data.Singletons (SingI)
import Data.Typeable (TypeRep, Typeable, cast, typeRep)
import GHC.Generics ((:*:), (:+:))
import qualified GHC.Generics as G

import Michelson.Typed.Aliases
import Michelson.Typed.Haskell.Value
import Michelson.Typed.T
import Michelson.Typed.Value
import Util.TypeLits

-- | Possible outcomes of an attempt to construct a Haskell ADT value
-- from constructor name and relevant data.
data ComposeResult a
  = ComposeOk a
    -- ^ Composed fine.
  | ComposeCtorNotFound
    -- ^ No constructor with such name.
  | ComposeFieldTypeMismatch TypeRep TypeRep
    -- ^ Found required constructor, but type of data does not correspond
    -- to provided one.
  deriving stock (Functor)

instance Semigroup (ComposeResult a) where
  r@(ComposeOk _) <> _ = r
  _ <> r@(ComposeOk _) = r
  r@(ComposeFieldTypeMismatch _ _) <> _ = r
  _ <> r@(ComposeFieldTypeMismatch _ _) = r
  r@ComposeCtorNotFound <> ComposeCtorNotFound = r

instance Monoid (ComposeResult a) where
  mempty = ComposeCtorNotFound
  mappend = (<>)

-- | Constraint for 'hsDecompose' and 'hsCompose'.
type LooseSumC dt =
  ( Generic dt, GLooseSum (G.Rep dt)
  )

-- | Decompose Haskell type into constructor name and
-- data it carries, converting the latter into Michelson 'Value'.
toTaggedVal :: LooseSumC dt => dt -> (Text, SomeValue)
toTaggedVal = gHsDecompose . G.from

-- | Inverse to 'toTaggedVal'.
fromTaggedVal :: LooseSumC dt => (Text, SomeValue) -> ComposeResult dt
fromTaggedVal = fmap G.to . gHsCompose

-- | Generic traversal for 'hsCompose' and 'hsDecompose'.
class GLooseSum (x :: Kind.Type -> Kind.Type) where
  gHsDecompose :: x p -> (Text, SomeValue)
  gHsCompose :: (Text, SomeValue) -> ComposeResult (x p)

instance GLooseSum x => GLooseSum (G.D1 i x) where
  gHsDecompose = gHsDecompose . G.unM1
  gHsCompose = fmap G.M1 . gHsCompose

instance (GLooseSum x, GLooseSum y) => GLooseSum (x :+: y) where
  gHsDecompose = \case
    G.L1 x -> gHsDecompose x
    G.R1 y -> gHsDecompose y
  gHsCompose v = mconcat
    [ G.L1 <$> gHsCompose v
    , G.R1 <$> gHsCompose v
    ]

instance (GAccessField x, KnownSymbol ctor) =>
         GLooseSum (G.C1 ('G.MetaCons ctor f o) x) where
  gHsDecompose = (symbolValT' @ctor, ) . gExtractField @x . G.unM1
  gHsCompose (ctor, val)
    | symbolValT' @ctor == ctor = G.M1 <$> gMakeField @x val
    | otherwise = ComposeCtorNotFound

instance GLooseSum G.V1 where
  gHsDecompose = \case{}
  gHsCompose _ = mempty

-- | Pick a field from constructor with zero or one fields.
class GAccessField (x :: Kind.Type -> Kind.Type) where
  gExtractField :: x p -> SomeValue
  gMakeField :: SomeValue -> ComposeResult (x p)

instance GAccessField x => GAccessField (G.S1 i x) where
  gExtractField = gExtractField . G.unM1
  gMakeField v = G.M1 <$> gMakeField @x v

instance (Typeable a, IsoValue a, Typeable (ToT a), SingI (ToT a)) =>
         GAccessField (G.Rec0 a) where
  gExtractField = SomeValue . toVal . G.unK1
  gMakeField (SomeValue v) = G.K1 . fromVal <$> composeCast v

instance GAccessField G.U1 where
  gExtractField G.U1 = SomeValue $ toVal ()
  gMakeField (SomeValue v) = G.U1 <$ composeCast @_ @'TUnit v

composeCast :: forall a b. (Typeable a, Typeable b) => Value a -> ComposeResult (Value b)
composeCast a =
  case cast a of
    Nothing -> ComposeFieldTypeMismatch (typeRep (Proxy @a)) (typeRep (Proxy @b))
    Just b -> ComposeOk b

instance
  TypeError ('Text "Cannot compose/decompose constructors with more \
             \than one field" ':$$:
             'Text "Consider using tuple instead") =>
  GAccessField (x :*: y) where
  gExtractField = error "impossible"
  gMakeField = error "impossible"
