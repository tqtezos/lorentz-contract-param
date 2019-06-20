module Lorentz.UStore.Common
  ( fieldNameToMText
  ) where

import GHC.TypeLits (KnownSymbol, symbolVal)

import Michelson.Text

fieldNameToMText :: forall field. KnownSymbol field => MText
fieldNameToMText =
  -- Using 'mkMTextUnsafe' because our coding practices does not allow
  -- weird characters (like unicode) in field names
  mkMTextUnsafe . toText . symbolVal $ Proxy @field
