-- | General type utilities.
module Util.Type
  ( IsElem
  , Guard
  , AllUnique
  , RequireAllUnique
  , ReifyList (..)
  ) where

import Data.Type.Bool (Not, type (&&), If)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage (..))

type family IsElem (a :: k) (l :: [k]) :: Bool where
  IsElem _ '[] = 'False
  IsElem a (a ': _) = 'True
  IsElem a (_ ': as) = IsElem a as

type family Guard (cond :: Bool) (a :: k) :: Maybe k where
  Guard 'False _ = 'Nothing
  Guard 'True a = 'Just a

type family AllUnique (l :: [k]) :: Bool where
  AllUnique '[] = 'True
  AllUnique (x : xs) = Not (IsElem x xs) && AllUnique xs

type family RequireAllUnique (desc :: Symbol) (l :: [k]) :: Constraint where
  RequireAllUnique _ '[] = ()
  RequireAllUnique desc (x : xs) =
    If (IsElem x xs)
       (TypeError ('Text "Duplicated " ':<>: 'Text desc ':<>: 'Text ":" ':$$:
                   'ShowType x)
       )
       (RequireAllUnique desc xs)

-- | Bring type-level list at term-level using given function
-- to demote its individual elements.
class ReifyList (c :: k -> Constraint) (l :: [k]) where
  reifyList :: (forall a. c a => Proxy a -> r) -> [r]

instance ReifyList c '[] where
  reifyList _ = []

instance (c x, ReifyList c xs) => ReifyList c (x ': xs) where
  reifyList reifyElem = reifyElem (Proxy @x) : reifyList @_ @c @xs reifyElem
