module Util.Lens
  ( postfixLFields
  ) where

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)

-- | For datatype with "myNyan" field it will create "myNyanL" lens.
postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])
