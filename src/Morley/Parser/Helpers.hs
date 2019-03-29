module Morley.Parser.Helpers
  ( sepEndBy1
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as P

sepEndBy1 :: MonadPlus m => m a -> m sep -> m (NonEmpty a)
sepEndBy1 = fmap NE.fromList ... P.sepEndBy1
