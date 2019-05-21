-- | General type utilities.
module Util.Type
  ( IsElem
  ) where

type family IsElem (a :: k) (l :: [k]) :: Bool where
  IsElem _ '[] = 'False
  IsElem a (a ': _) = 'True
  IsElem a (_ ': as) = IsElem a as
