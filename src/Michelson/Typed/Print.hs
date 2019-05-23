{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types printing.
--
-- Moved in a separate module, because we print via converting to
-- untyped @T@ type to avoid duplication.
module Michelson.Typed.Print
  ( buildStack
  ) where

import Fmt (Buildable(..), Builder, listF)

import Michelson.Typed.Extract
import Michelson.Typed.T

instance Buildable T where
  build = build . toUType

-- | Format type stack in a pretty way.
buildStack :: [T] -> Builder
buildStack = listF
