-- | Re-exports 'GHC.TypeLits', modifying it considering our practices.
module Util.TypeLits
  ( KnownSymbol
  , symbolValT
  , symbolValT'

  , TypeError
  , ErrorMessage (..)
  ) where

import GHC.TypeLits

symbolValT :: forall s. KnownSymbol s => Proxy s -> Text
symbolValT = toText . symbolVal

symbolValT' :: forall s. KnownSymbol s => Text
symbolValT' = symbolValT (Proxy @s)
