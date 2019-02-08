{-# OPTIONS_GHC -Wno-orphans #-}
module Morley.Default
  ( permute2Def , permute3Def
  , Default (..)
  ) where

import Control.Applicative.Permutations
import Data.Default

{- Permutation Parsers -}

permute2Def :: (Default a, Default b, Monad f, Alternative f) => f a -> f b -> f (a,b)
permute2Def a b = runPermutation $
  (,) <$> toPermutationWithDefault def a
      <*> toPermutationWithDefault def b

permute3Def :: (Default a, Default b, Default c, Monad f, Alternative f) =>
                f a -> f b -> f c -> f (a,b,c)
permute3Def a b c = runPermutation $
  (,,) <$> toPermutationWithDefault def a
       <*> toPermutationWithDefault def b
       <*> toPermutationWithDefault def c
