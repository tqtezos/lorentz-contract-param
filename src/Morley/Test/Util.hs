-- | Testing utility functions used by testing framework itself or
-- intended to be used by test writers.

module Morley.Test.Util
  ( failedProp
  , succeededProp
  , qcIsLeft
  , qcIsRight
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

-- | The 'Property' holds on `Left a`.
qcIsLeft :: Show b => Either a b -> Property
qcIsLeft (Left _)  = property True
qcIsLeft (Right x) = failedProp $ "expected Left, got Right (" <> show x <> ")"

-- | The 'Property' holds on `Right b`.
qcIsRight :: Show a => Either a b -> Property
qcIsRight (Right _) = property True
qcIsRight (Left x)  = failedProp $ "expected Right, got Left (" <> show x <> ")"
