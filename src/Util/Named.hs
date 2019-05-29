{-# OPTIONS_GHC -Wno-orphans #-}

-- | Additional functionality for 'named' package.
module Util.Named
  ( (.!)
  , (.?)
  , (<.!>)
  , (<.?>)
  , NamedInner
  ) where

import Control.Lens (Wrapped(..), iso)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Named (Name, NamedF(..))
import qualified Text.Show

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

instance Wrapped (NamedF Identity a name) where
  type Unwrapped (NamedF Identity a name) = a
  _Wrapped' = iso (\(ArgF a) -> runIdentity a) (ArgF . Identity)

instance Wrapped (NamedF Maybe a name) where
  type Unwrapped (NamedF Maybe a name) = Maybe a
  _Wrapped' = iso (\(ArgF a) -> a) ArgF

deriving instance Eq a => Eq (NamedF Identity a name)

instance (Show a, KnownSymbol name) => Show (NamedF Identity a name) where
  show (ArgF a) = symbolVal (Proxy @name) <> " :! " <> show a
