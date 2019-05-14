{-# OPTIONS_GHC -Wno-orphans #-}

-- | Missing instances from libraries.
module Util.Instances () where

import Data.Default (Default(..))

instance Default Natural where
  def = 0
