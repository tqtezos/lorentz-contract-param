module Michelson.Typed.Haskell.Instr.Helpers
  ( Branch (..)
  , Path
  , RSplit (..)
  ) where

import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.TypeLevel (type (++))

-- | Which branch to choose in generic tree representation.
data Branch = L | R

-- | Path to a leaf (some field or constructor) in generic tree representation.
type Path = [Branch]

-- | Split a record into two pieces.
class RSplit (l :: [k]) (r :: [k]) where
  rsplit :: forall f. Rec f (l ++ r) -> (Rec f l, Rec f r)

instance RSplit '[] r where
  rsplit = (RNil, )

instance RSplit ls r => RSplit (l ': ls) r where
  rsplit (x :& r) =
    let (x1, r1) = rsplit r
    in (x :& x1, r1)
