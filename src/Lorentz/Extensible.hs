{- | This module contains implementation of 'Extensible' values.

@Extensible@ values are an alternative representation of sum-types
for Michelson. Instead of representing them as nested options, we
treat them as (Natural, ByteString) pair, where the first element
of the pair represents the constructor index, while the second is
a packed argument.

With such a representation sum types can be easily upgraded: it is
possible to add new elements to the sum type, and the representation
would not change.

However, such representation essentially limits the applicability of
the values. This module does not provide Michelson-level function to
unwrap the value because it would require traversing all the possible
options in the contract code. While this is possible, it is very
inefficient. Up to this moment, we have not come up with a decent
reason to allow such behavior, so Extensible types are write-only
in Michelson code. They can be unwrapped off-chain with @fromExtVal@.

In order to preserve previous values during migrations, users should
ONLY APPEND items to the underlying sum type. Changing, reordering and
deleting items is not allowed and would lead to compatibility breakage.
Currently, this restriction in not enforced. Only no-argument and
one-argument constructors are supported.

GOOD:
  -- `Extensible GoodSumTypeV1` is backwards compatible
  -- with `Extensible GoodSumTypeV2`
  data GoodSumTypeV1 = A Natural | B
  data GoodSumTypeV2 = A Natural | B | C MText

BAD:
  -- `Extensible BadSumTypeV1` is NOT backwards compatible
  -- with `Extensible BadSumTypeV2`
  data BadSumTypeV1 = A | B
  data BadSumTypeV2 = A Natural | B | C MText
-}

{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Lorentz.Extensible
  ( Extensible (..)
  , ExtConversionError (..)
  , ExtVal
  , toExtVal
  , fromExtVal
  , wrapExt
  ) where

import qualified Data.Kind as Kind
import Data.Typeable (Proxy(..))
import Data.Vinyl.Derived (Label)
import Data.Vinyl.TypeLevel (type (++))
import Fmt (Buildable(build), (+||), (||+))
import GHC.Generics ((:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (AppendSymbol, Nat, Symbol)
import GHC.TypeNats (type (+))

import Lorentz.Base
import Lorentz.Coercions
import Lorentz.Constraints
import Lorentz.Instr
import Michelson.Typed
import Michelson.Interpret.Pack
import Michelson.Interpret.Unpack
import Util.TypeLits

newtype Extensible x = Extensible (Natural, ByteString)
  deriving stock (Generic, Eq, Show)
  deriving anyclass (IsoValue)

type ExtVal x = (Generic x, GExtVal x (G.Rep x))
type GetCtors x = GGetCtors (G.Rep x)

-- | Converts a value from a Haskell representation to its
--   extensible Michelson representation (i.e. (Natural, Bytestring) pair).
toExtVal :: ExtVal a => a -> Extensible a
toExtVal = gToExtVal . G.from

-- | Converts a value from an extensible Michelson representation to its
--   Haskell sum-type representation. Fails if the Michelson representation
--   points to a nun-existent constructor, or if we failed to unpack
--   the argument.
fromExtVal :: ExtVal a => UnpackEnv -> Extensible a -> Either ExtConversionError a
fromExtVal env val = fmap G.to $ gFromExtVal env val

-- | Wraps an argument on top of the stack into an Extensible representation
wrapExt
  :: forall t (n :: Nat) name param s.
  ( Ctor n name param ~ LookupCtor name (GetCtors t)
  , KnownNat n
  , IsoValue param, KnownValue param, NoOperation param, NoBigMap param
  )
  => Label ("c" `AppendSymbol` name) -> param ': s :-> Extensible t ': s
wrapExt _ = pack # push (natVal (Proxy @n)) # pair # coerce_

-- | Errors related to fromExtVal conversion
data ExtConversionError
  = ConstructorIndexNotFound Natural
  | ArgumentUnpackFailed
  deriving stock (Eq, Show)

instance Buildable ExtConversionError where
  build =
    \case
      ConstructorIndexNotFound idx ->
        "Could not convert Extensible value into its Haskell representation: \
        \constructor #" +|| idx ||+ " was not found in the sum type \
        \constructors list"
      ArgumentUnpackFailed ->
        "Could not convert Extensible value into its Haskell representation: \
        \failed to unpack constructor argument"

data Position (n :: Nat)
data Ctor (n :: Nat) (name :: Symbol) (param :: Kind.Type)
type CtorKind = (Symbol, Kind.Type)

-- | Finds the constructor's position and argument type by its name
type LookupCtor (name :: Symbol) (entries :: [CtorKind])
  = LookupCtorImpl (Position 0) name entries

type family LookupCtorImpl (pos :: Kind.Type) (name :: Symbol) (entries :: [CtorKind])
             :: Kind.Type where
  LookupCtorImpl (Position n) name ('(name, param) ': _) = Ctor n name param
  LookupCtorImpl (Position n) name (_ ': entries) =
    LookupCtorImpl (Position (n + 1)) name entries
  LookupCtorImpl _ name '[] =
    TypeError ('Text "Constructor " ':<>: 'ShowType name ':<>:
               'Text " is not in the sum type constructor list")

-- | Having a sum-type, yields a type-level list of its constructors
type family GGetCtors (x :: Kind.Type -> Kind.Type) :: [CtorKind] where
  GGetCtors (G.D1 _ x) = GGetCtors x
  GGetCtors (G.C1 ('G.MetaCons name _1 _2) (G.S1 _3 (G.Rec0 param)))
    = '[ '(name, param) ]
  GGetCtors (G.C1 ('G.MetaCons name _1 _2) G.U1)
    = '[ '(name, ()) ]
  GGetCtors (x :+: y) = GGetCtors x ++ GGetCtors y

-- | Generic implementation of toExtVal and fromExtVal
class GExtVal t (x :: Kind.Type -> Kind.Type) where
  gToExtVal :: x p -> Extensible t
  gFromExtVal :: UnpackEnv -> Extensible t -> Either ExtConversionError (x p)

instance GExtVal t x => GExtVal t (G.D1 i x) where
  gToExtVal = gToExtVal @t . G.unM1
  gFromExtVal env val = fmap G.M1 (gFromExtVal @t env val)

instance ( Ctor n name () ~ LookupCtor name (GetCtors t)
         , KnownNat n
         )
         => GExtVal t (G.C1 ('G.MetaCons name _1 _2) G.U1) where
  gToExtVal (G.M1 G.U1) = Extensible
    ( natVal (Proxy @n)
    , packValue' $ toVal ()
    )
  gFromExtVal _ (Extensible (idx, _))
    | idx == natVal (Proxy @n)
        = Right $ G.M1 G.U1
    | otherwise = Left $ ConstructorIndexNotFound idx

instance ( IsoValue param, KnownValue param, NoOperation param, NoBigMap param
         , Ctor n name param ~ LookupCtor name (GetCtors t)
         , KnownNat n
         )
         => GExtVal t (G.C1 ('G.MetaCons name _1 _2) (G.S1 _3 (G.Rec0 param))) where
  gToExtVal (G.M1 (G.M1 (G.K1 param))) = Extensible
    ( natVal (Proxy @n)
    , forbiddenOp @(ToT param) $ forbiddenBigMap @(ToT param) $
      packValue' $ toVal param
    )
  gFromExtVal env (Extensible (idx, bs))
    | idx == natVal (Proxy @n)
        = forbiddenOp @(ToT param) $ forbiddenBigMap @(ToT param) $
          first (\_ -> ArgumentUnpackFailed) $
          fmap (G.M1 . G.M1 . G.K1 . fromVal) $ unpackValue' @(ToT param) env bs
    | otherwise = Left $ ConstructorIndexNotFound idx

instance (GExtVal t x, GExtVal t y) => GExtVal t (x :+: y) where
  gToExtVal = \case
    G.L1 x -> let Extensible val = gToExtVal @t x in Extensible val
    G.R1 y -> let Extensible val = gToExtVal @t y in Extensible val
  gFromExtVal env val =
    let l = fmap G.L1 (gFromExtVal @t env val)
        r = fmap G.R1 (gFromExtVal @t env val)
    in l <> r
