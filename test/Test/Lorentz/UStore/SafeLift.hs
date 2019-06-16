-- | We have a constraint in 'ustoreLift' which
-- forbids nested store templates with duplicated fields.
-- This module checks this constraint will work fine.
module Test.Lorentz.UStore.SafeLift
  ( test_UStore_lift
  ) where

import Data.Typeable ((:~:)(..))
import Test.Tasty (TestTree)

import Lorentz.UStore
import Lorentz.UStore.Lift

import Test.Lorentz.UStore.SafeLift.Helpers

-- Fake tests to deceive "weeder".
-- All the check consist of typechecking some stuff, see below.
test_UStore_lift :: [TestTree]
test_UStore_lift = []

_checkDuplicates0 :: UStoreFieldsAreUnique MySimpleTemplate :~: 'True
_checkDuplicates0 = Refl

data MyTemplateBig = MyTemplateBig
  { ints :: Integer |~> Natural
  , small :: MySimpleTemplate
  } deriving stock (Generic)

_checkDuplicates1 :: UStoreFieldsAreUnique MyTemplateBig :~: 'False
_checkDuplicates1 = Refl

data MyTemplate2 = MyTemplate2
  { bool :: UStoreField Bool
  } deriving Generic

data MyTemplateSuperBig = MyTemplateSuperBig
  { ssmall :: MySimpleTemplate
  , ssmall2 :: MyTemplate2
  } deriving Generic

_checkDuplicates2 :: UStoreFieldsAreUnique MyTemplateSuperBig :~: 'False
_checkDuplicates2 = Refl
