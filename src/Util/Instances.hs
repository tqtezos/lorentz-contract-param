{-# OPTIONS_GHC -Wno-orphans #-}

-- | Missing instances from libraries.
module Util.Instances () where

import Data.Default (Default(..))
import Fmt (Buildable(..))

instance Default Natural where
  def = 0

instance Buildable Natural where
  build = build @Integer . fromIntegral
