{- | Reimplementation of some syntax sugar.

You need the following module pragmas to make it work smoothly:

{-# LANGUAGE NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-}
module Lorentz.Rebinded
  ( (>>)
  , pure
  , return

    -- * Re-exports required for RebindableSyntax
  , fromInteger
  , fromLabel
  ) where


import Prelude hiding ((>>), (>>=))

import Lorentz.Base

-- | Aliases for '(#)' used by do-blocks.
(>>) :: (a :-> b) -> (b :-> c) -> (a :-> c)
(>>) = (#)
