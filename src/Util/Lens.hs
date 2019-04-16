module Util.Lens
  ( HasLens (..)
  ) where

class HasLens a s where
  lensOf :: Lens' s a

instance HasLens a a where
  lensOf = id
