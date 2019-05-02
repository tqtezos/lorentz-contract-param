-- | Generic-related utils.
module Util.Generic
  ( mkGenericTree
  , mkGenericTreeVec
  ) where

import Control.Exception (assert)
import qualified Data.Vector as V

-- | Rebuild a list into a binary tree of exactly the same form which
-- 'Data.Generic' uses to represent datatypes.
mkGenericTree :: (a -> a -> a) -> NonEmpty a -> a
mkGenericTree mkNode = mkGenericTreeVec id mkNode . V.fromList . toList

mkGenericTreeVec :: HasCallStack => (a -> b) -> (b -> b -> b) -> V.Vector a -> b
mkGenericTreeVec mkLeaf mkNode vector
  | V.null vector = error "Empty vector"
  | otherwise = mkTreeDo vector
  where
    mkTreeDo es
      | V.length es == 1 = mkLeaf $ V.head es
      | otherwise = assert (V.length es > 1) $
          let mid = V.length es `div` 2
              (h, t) = V.splitAt mid es
          in mkNode (mkTreeDo h) (mkTreeDo t)
