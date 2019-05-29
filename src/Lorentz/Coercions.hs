-- | Identity transformations between different Haskell types.
module Lorentz.Coercions
  ( Coercible_
  , coerce_

  , coerceUnwrap
  , coerceWrap
  , toNamed
  , fromNamed
  ) where

import Control.Lens (Wrapped(..))
import Data.Vinyl.Derived (Label)
import Named (NamedF)

import Lorentz.Base
import Michelson.Typed

-- | Whether two types have the same Michelson representation.
type Coercible_ a b = ToT a ~ ToT b

-- | Convert between values of types that have the same representation.
coerce_ :: Coercible_ a b => a & s :-> b & s
coerce_ = I Nop

-- | Specialized version of 'coerce_' to wrap into a haskell newtype.
coerceWrap
  :: Coercible_ newtyp (Unwrapped newtyp)
  => Unwrapped newtyp : s :-> newtyp : s
coerceWrap = coerce_

-- | Specialized version of 'coerce_' to unwrap a haskell newtype.
coerceUnwrap
  :: Coercible_ newtyp (Unwrapped newtyp)
  => newtyp : s :-> Unwrapped newtyp : s
coerceUnwrap = coerce_

-- | Lift given value to a named value.
toNamed :: Label name -> a : s :-> NamedF Identity a name : s
toNamed _ = coerceWrap

-- | Unpack named value.
fromNamed :: Label name -> NamedF Identity a name : s :-> a : s
fromNamed _ = coerceUnwrap
