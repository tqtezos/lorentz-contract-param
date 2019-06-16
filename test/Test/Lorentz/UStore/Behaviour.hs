{-# LANGUAGE DerivingStrategies #-}

-- | Tests for Lorentz 'UStore'.
module Test.Lorentz.UStore.Behaviour
  ( test_Roundtrip
  , test_Conversions
  ) where

import Data.Default (def)
import qualified Data.Map as M
import Data.Vinyl.Core (Rec(..))
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck (Arbitrary(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz.Base
import Lorentz.Instr as L
import Lorentz.UStore
import Michelson.Interpret.Unpack
import Michelson.Test.Dummy
import Michelson.Test.Util
import Util.Test.Arbitrary ()

import Test.Util.QuickCheck (roundtripTest)


data MyTemplate = MyTemplate
  { ints :: Integer |~> ()
  , bool :: UStoreField Bool
  } deriving stock (Eq, Show, Generic)

instance Arbitrary MyTemplate where
  arbitrary = MyTemplate <$> arbitrary <*> arbitrary

data MyTemplateBig = MyTemplateBig
  { small :: MyTemplate
  , bytes :: ByteString |~> Natural
  , total :: UStoreField Integer
  } deriving stock (Eq, Show, Generic)

instance Arbitrary MyTemplateBig where
  arbitrary = MyTemplateBig <$> arbitrary <*> arbitrary <*> arbitrary

test_Roundtrip :: [TestTree]
test_Roundtrip =
  [ roundtripTest (mkUStore @MyTemplate) (ustoreDecomposeFull dummyUnpackEnv)
  , roundtripTest (mkUStore @MyTemplateBig) (ustoreDecomposeFull dummyUnpackEnv)
  ]

test_Conversions :: [TestTree]
test_Conversions =
  [ testGroup "Simple store template"
    [ testCase "No action" $
        ustoreChangeTest
          ( nop
          , MyTemplate (UStoreSubMap def) (UStoreField False)
          , MyTemplate (UStoreSubMap def) (UStoreField False)
          )
    , testCase "Insert into submap" $
        ustoreChangeTest
          ( unit # push 5 # ustoreInsert #ints
          , MyTemplate (UStoreSubMap def) (UStoreField False)
          , MyTemplate (UStoreSubMap $ one (5, ())) (UStoreField False)
          )
    , testCase "Delete from submap" $
        ustoreChangeTest
          ( push 3 # ustoreDelete #ints
          , MyTemplate (UStoreSubMap $ one (3, ())) (UStoreField False)
          , MyTemplate (UStoreSubMap mempty) (UStoreField False)
          )
    , testCase "Get from submap" $
        ustoreChangeTest
          ( dup # push 0 # ustoreGet #ints #
            ifNone (push 10) (L.drop # push 11) # dip unit # ustoreInsert #ints
          , MyTemplate (UStoreSubMap $ one (0, ())) (UStoreField False)
          , MyTemplate (UStoreSubMap $ M.fromList [(0, ()), (11, ())]) (UStoreField False)
          )
    , testCase "Set field" $
        ustoreChangeTest
          ( push True # ustoreSetField #bool
          , MyTemplate (UStoreSubMap mempty) (UStoreField False)
          , MyTemplate (UStoreSubMap mempty) (UStoreField True)
          )
    , testCase "Get field" $
        ustoreChangeTest
          ( ustoreGetField #bool #
            if_ (push 5) (push 0) # dip unit # ustoreInsert #ints
          , MyTemplate (UStoreSubMap mempty) (UStoreField False)
          , MyTemplate (UStoreSubMap $ one (0, ())) (UStoreField False)
          )
    , testCase "Leave some entries untouched" $
        ustoreChangeTest
          ( push 0 # ustoreDelete #ints #
            unit # push 2 # ustoreInsert #ints
          , MyTemplate (UStoreSubMap $ M.fromList [(0, ()), (1, ())]) (UStoreField False)
          , MyTemplate (UStoreSubMap $ M.fromList [(1, ()), (2, ())]) (UStoreField False)
          )
    ]

  , testGroup "Non-flat store template"
    [ testCase "Custom scenario 1" $
        ustoreChangeTest
          ( push "a" # ustoreDelete #bytes #
            push 2 # push "b" # ustoreInsert #bytes #
            ustoreGetField #total # push @Integer 1 # add # ustoreSetField #total #
            unliftUStore #small #
            unit # push 0 # ustoreInsert #ints #
            push True # ustoreSetField #bool #
            liftUStore #small
          , MyTemplateBig
            { small = MyTemplate (UStoreSubMap def) (UStoreField False)
            , bytes = UStoreSubMap $ one ("a", 1)
            , total = UStoreField 10
            }
          , MyTemplateBig
            { small = MyTemplate (UStoreSubMap $ one (0, ())) (UStoreField True)
            , bytes = UStoreSubMap $ one ("b", 2)
            , total = UStoreField 11
            }
          )
    ]
  ]
  where
    -- We accept a tuple as argument to avoid many parentheses
    ustoreChangeTest
      :: (Each [Eq, Show, Generic, UStoreConversible] '[template], HasCallStack)
      => ( '[UStore template] :-> '[UStore template]
         , template
         , template
         )
      -> Assertion
    ustoreChangeTest (instr, initStoreHs, expectedNewStore) =
      let
        initStore = mkUStore initStoreHs
        (Identity ustore :& RNil) =
          leftToPrettyPanic $
          interpretLorentzInstr dummyContractEnv instr (Identity initStore :& RNil)
      in case ustoreDecomposeFull dummyUnpackEnv ustore of
          Left err -> assertFailure (toString err)
          Right ustoreHs -> ustoreHs @?= expectedNewStore
