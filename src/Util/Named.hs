-- | Additional functionality for 'named' package.
module Util.Named
  ( (.!)
  , (.?)
  , (<.!>)
  , (<.?>)
  , NamedInner
  ) where

import Named (Name, NamedF(..))

(.!) :: Name name -> a -> NamedF Identity a name
(.!) _ = ArgF . Identity

(.?) :: Name name -> Maybe a -> NamedF Maybe a name
(.?) _ = ArgF

(<.!>) :: Functor m => Name name -> m a -> m (NamedF Identity a name)
(<.!>) name = fmap (name .!)

(<.?>) :: Functor m => Name name -> m (Maybe a) -> m (NamedF Maybe a name)
(<.?>) name = fmap (name .?)

type family NamedInner n where
  NamedInner (NamedF Identity a _) = a
  NamedInner (NamedF Maybe a _) = Maybe a
