-- | General type utilities.
module Util.Type
  ( IsElem
  , Guard
  , AllUnique
  ) where

import Data.Type.Bool (Not, type (&&))

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
