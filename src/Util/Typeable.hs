-- | Utility for 'Typeable'.
module Util.Typeable
  ( gcastE
  ) where

import Data.Typeable (Typeable, gcast, typeRep)
import Fmt ((+||), (||+))

-- | Like 'gcast', casts some container's elements,
-- producing informative error on mismatch.
gcastE :: forall a b t. (Typeable a, Typeable b) => t a -> Either Text (t b)
gcastE = maybeToRight errMsg . gcast
  where
    errMsg = "Type mismatch: expected " +|| typeRep (Proxy @b) ||+
                               ", got " +|| typeRep (Proxy @a) ||+ ""
