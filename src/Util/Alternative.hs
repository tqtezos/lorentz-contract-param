-- | Utilities related to 'Alternative'.

module Util.Alternative
  ( someNE
  ) where

import qualified Data.List.NonEmpty as NE

-- | This function is the same as 'some' except that it returns
-- 'NonEmpty', because 'some' is guaranteed to return non-empty list,
-- but it's not captured in types.
someNE :: Alternative f => f a -> f (NonEmpty a)
someNE = fmap NE.fromList . some
