-- | Testing utility functions used by testing framework.

module Morley.Test.Util
  ( failedProp
  , succeededProp
  ) where

import Test.QuickCheck.Property (Property, Result(..), failed, property)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

-- | A 'Property' that always failes with given message.
failedProp :: Text -> Property
failedProp r = property $ failed { reason = toString r }

-- | A 'Property' that always succeeds.
succeededProp :: Property
succeededProp = property True
