-- | Additional functionality for 'named' package.
module Util.Named
  ( (.!)
  , (.?)
  , NamedInner
  ) where

import Named (Name, NamedF(..))

(.!) :: Name name -> a -> NamedF Identity a name
(.!) _ = ArgF . Identity

(.?) :: Name name -> a -> NamedF Maybe a name
(.?) _ = ArgF . Just

type family NamedInner n where
  NamedInner (NamedF Identity a _) = a
  NamedInner (NamedF Maybe a _) = Maybe a
