module Lorentz.Constraints
  ( KnownValue
  , KnownCValue
  , NoOperation
  ) where

import Data.Singletons (SingI)

import Michelson.Typed

-- | Gathers constraints, commonly required for values.
type KnownValue a = (Typeable (ToT a), SingI (ToT a))

type KnownCValue a = (IsoValue a, Typeable (ToCT a), SingI (ToCT a))

-- | Ensure given type does not contain "operation".
type NoOperation a = ForbidOp (ToT a)
