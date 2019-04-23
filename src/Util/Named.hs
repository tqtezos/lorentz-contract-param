-- | Additional functionality for 'named' package.
module Util.Named
  ( (.!)
  , (.?)
  ) where

import Named (Name, NamedF(..))

(.!) :: Name name -> a -> NamedF Identity a name
(.!) _ = ArgF . Identity

(.?) :: Name name -> a -> NamedF Maybe a name
(.?) _ = ArgF . Just
