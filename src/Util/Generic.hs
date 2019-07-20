-- | Generic-related utils.
module Util.Generic
  ( mkGenericTree
  , mkGenericTreeVec
  ) where

import Control.Exception (assert)
import qualified Data.Vector as V

-- | Rebuild a list into a binary tree of exactly the same form which
-- 'Data.Generic' uses to represent datatypes.
--
-- Along with the original list you have to provide constructor for intermediate
-- nodes - it accepts zero-based index of the leftmost element of the right tree
-- and merged trees themselves.
mkGenericTree :: (Natural -> a -> a -> a) -> NonEmpty a -> a
mkGenericTree mkNode = mkGenericTreeVec id mkNode . V.fromList . toList

mkGenericTreeVec
  :: HasCallStack
  => (a -> b) -> (Natural -> b -> b -> b) -> V.Vector a -> b
mkGenericTreeVec mkLeaf mkNode vector
  | V.null vector = error "Empty vector"
  | otherwise = mkTreeDo 0 vector
  where
    mkTreeDo idxBase es
      | V.length es == 1 = mkLeaf $ V.head es
      | otherwise = assert (V.length es > 1) $
          let mid = V.length es `div` 2
              mid' = idxBase + mid
              (h, t) = V.splitAt mid es
          in mkNode (fromIntegral mid') (mkTreeDo idxBase h) (mkTreeDo mid' t)
