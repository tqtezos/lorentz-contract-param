{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Type-nat utilities.
--
-- We take Peano numbers as base for operations because they make it
-- much easer to prove things to compiler. Their performance does not
-- seem to introduce a problem, because we use nats primarily along with
-- stack which is a linked list with similar performance characteristics.
--
-- Many of things we introduce here are covered in @type-natural@ package,
-- but unfortunatelly it does not work with GHC 8.6 at the moment of writing
-- this module. We use 'Data.Vinyl' as source of Peano @Nat@ for now.
module Util.Peano
  ( -- * General
    Peano
  , ToPeano
  , Length
  , KnownPeano (..)
  , peanoVal'
  , Sing (SZ, SS)
  , At

    -- * Morley-specific utils
  , IsLongerThan
  , LongerThan
  , RequireLongerThan
  , requiredLongerThan
  ) where

import Prelude hiding (Nat)

import Data.Singletons (Sing, SingI(..))
import qualified GHC.TypeNats as GHC (Nat)
import Unsafe.Coerce (unsafeCoerce)
import Data.Constraint (Dict (..))
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Data.Type.Bool (If)
import Data.Vinyl.TypeLevel (Nat(..), RLength)
import GHC.TypeNats (type (-), type (+))

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------

-- | A convenient alias.
--
-- We are going to use 'Peano' numbers for type-dependent logic and
-- normal 'Nat's in user API, need to distinguish them somehow.
type Peano = Nat

type family ToPeano (n :: GHC.Nat) :: Nat where
  ToPeano 0 = 'Z
  ToPeano a = 'S (ToPeano (a - 1))

type family FromPeano (n :: Nat) :: GHC.Nat where
  FromPeano 'Z = 0
  FromPeano ('S a) = 1 + FromPeano a

type family Length l where
  Length l = RLength l

class KnownPeano (n :: Nat) where
  peanoVal :: proxy n -> Natural

instance KnownPeano 'Z where
  peanoVal _ = 0
instance KnownPeano a => KnownPeano ('S a) where
  peanoVal _ = peanoVal' @a + 1

peanoVal' :: forall n. KnownPeano n => Natural
peanoVal' = peanoVal (Proxy @n)

data instance Sing (_ :: Nat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

deriving instance Eq (Sing (n :: Nat))

instance SingI 'Z where
  sing = SZ
instance SingI n => SingI ('S n) where
  sing = SS (sing @n)

type family At (n :: Peano) s where
  At 'Z (x ': _) = x
  At ('S n) (_ ': xs) = At n xs
  At a '[] =
    TypeError
      ('Text "You try to access to non-existing element of the stack, n = " ':<>:
         'ShowType (FromPeano a))

----------------------------------------------------------------------------
-- Morley-specific utils
----------------------------------------------------------------------------

-- | Comparison of type-level naturals, as a function.
--
-- It is as lazy on the list argument as possible - there is no
-- need to know the whole list if the natural argument is small enough.
-- This property is important if we want to be able to extract reusable
-- parts of code which are aware only of relevant part of stack.
type family IsLongerThan (l :: [k]) (a :: Nat) :: Bool where
  IsLongerThan (_ ': _) 'Z = 'True
  IsLongerThan (_ ': xs) ('S a) = IsLongerThan xs a
  IsLongerThan '[] _ = 'False

-- | Comparison of type-level naturals, as a constraint.
type LongerThan l a = IsLongerThan l a ~ 'True

{- | Evaluates list length.

This type family is a best-effort attempt to display neat error messages
when list is known only partially.

For instance, when called on @Int ': Int ': s@, the result will be
@OfLengthWithTail 2 s@ - compare with result of simple 'Length' -
@1 + 1 + Length s@.

For concrete types this will be identical to calling @FromPeano (Length l)@.
-}
type family OfLengthWithTail (acc :: GHC.Nat) (l :: [k]) :: GHC.Nat where
  OfLengthWithTail a '[] = a
  OfLengthWithTail a (_ ': xs) = OfLengthWithTail (a + 1) xs

type LengthWithTail l = OfLengthWithTail 0 l

-- | Comparison of type-level naturals, raises human-readable compile error
-- when does not hold.
--
-- This is for in eDSL use only, GHC cannot reason about such constraint.
type family RequireLongerThan (l :: [k]) (a :: Nat) :: Constraint where
  RequireLongerThan l a =
    If (IsLongerThan l a)
       (() :: Constraint)
       (TypeError
          ('Text "Stack element #" ':<>: 'ShowType (FromPeano a) ':<>:
           'Text " is not accessible" ':$$:
           'Text "Current stack has size of only " ':<>:
           'ShowType (LengthWithTail l) ':<>:
           'Text ":" ':$$: 'ShowType l
           ))

-- | Derive 'LongerThan' from 'RequireLongerThan'.
requiredLongerThan
  :: forall l a r. (RequireLongerThan l a)
  => (LongerThan l a => r) -> r
requiredLongerThan r = do
  -- It's not clear now to proof GHC this implication so we use
  -- @unsafeCoerce@ below and also disable "-Wredundant-constraints" extension.
  case unsafeCoerce @(Dict (LongerThan '[Int] 'Z)) @(Dict (LongerThan l a)) Dict of
    Dict -> r
