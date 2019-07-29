-- | Generic-related utils.
module Util.Generic
  ( mkGenericTree
  , mkGenericTreeVec

  , GenericTypeName
  ) where

import Control.Exception (assert)
import qualified Data.Kind as Kind
import qualified Data.Vector as V
import qualified GHC.Generics as G
import GHC.TypeLits (Symbol)

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

-- | Extract datatype name via its Generic representation.
--
-- For polymorphic types this throws away all type arguments.
type GenericTypeName a = GTypeName (G.Rep a)

type family GTypeName (x :: Kind.Type -> Kind.Type) :: Symbol where
  GTypeName (G.D1 ('G.MetaData tyName _ _ _) _) = tyName
