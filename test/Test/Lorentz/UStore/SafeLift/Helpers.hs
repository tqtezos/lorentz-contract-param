-- | Contains a template for safe lifting check.
--
-- We are going to define another template with the same field name,
-- so putting this template in a separate module.
module Test.Lorentz.UStore.SafeLift.Helpers
  ( MySimpleTemplate (..)
  ) where

import Lorentz.UStore

data MySimpleTemplate = MySimpleTemplate
  { ints :: Integer |~> ()
  , bool :: UStoreField Bool
  } deriving Generic
