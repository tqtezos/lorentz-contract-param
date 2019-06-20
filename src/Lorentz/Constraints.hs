{-# LANGUAGE UndecidableSuperClasses #-}

module Lorentz.Constraints
  ( CanHaveBigMap
  , KnownValue
  , KnownCValue
  , NoOperation
  , NoBigMap
  ) where

import Data.Singletons (SingI)

import Michelson.Typed

-- We write these constraints as class + instance, rather than
-- type aliases, in order to allow their partial application.

-- | Gathers constraints, commonly required for values.
class (Typeable (ToT a), SingI (ToT a)) => KnownValue a
instance (Typeable (ToT a), SingI (ToT a)) => KnownValue a

class (IsoValue a, Typeable (ToCT a), SingI (ToCT a)) => KnownCValue a
instance (IsoValue a, Typeable (ToCT a), SingI (ToCT a)) => KnownCValue a

-- | Ensure given type does not contain "operation".
class ForbidOp (ToT a) => NoOperation a
instance ForbidOp (ToT a) => NoOperation a

class ForbidBigMap (ToT a) => NoBigMap a
instance ForbidBigMap (ToT a) => NoBigMap a

class AllowBigMap (ToT a) => CanHaveBigMap a
instance AllowBigMap (ToT a) => CanHaveBigMap a
