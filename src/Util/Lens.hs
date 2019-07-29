module Util.Lens
  ( postfixLFields
  , makeLensesWith
  ) where

import Control.Lens (LensRules, lensField, lensRules, makeLensesWith, mappingNamer)

-- | For datatype with "myNyan" field it will create "myNyanL" lens.
postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])
