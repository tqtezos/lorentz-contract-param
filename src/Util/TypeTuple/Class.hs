module Util.TypeTuple.Class
  ( RecFromTuple (..)
  ) where

import qualified Data.Kind as Kind

-- | Building a record from tuple.
--
-- It differs from similar typeclass in 'Data.Vinyl.FromTuple' module in that
-- it allows type inference outside-in - knowing desired 'Rec' you know which
-- tuple should be provided - this improves error messages when constructing
-- concrete 'Rec' objects.
class RecFromTuple r where
  type IsoRecTuple r :: Kind.Type
  recFromTuple :: IsoRecTuple r -> r
