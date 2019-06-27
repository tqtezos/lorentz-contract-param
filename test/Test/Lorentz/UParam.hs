{-# LANGUAGE DerivingStrategies #-}

-- | Tests for Lorentz 'UParam'.
module Test.Lorentz.UParam
  ( test_Simple_contract
  ) where

import Data.Vinyl.Core (Rec(..))
import Test.HUnit ((@?=), assertBool)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Michelson.Text
import Lorentz ((/->))
import qualified Lorentz as L
import Lorentz.Base
import Lorentz.UParam
import Michelson.Test.Dummy

-- Test on simple contract
----------------------------------------------------------------------------

type Entries = ["add" ?: Natural, "id" ?: ()]

simpleCode :: '[UParam Entries, Integer] :-> '[Integer]
simpleCode = caseUParamT
  ( #add /-> L.add
  , #id /-> L.drop @()
  ) uparamFallbackFail

test_Simple_contract :: [TestTree]
test_Simple_contract =
  [ testCase "Passing parameter 1" $
      runSimpleCode 5 (mkUParam #add 3) @?= Right 8
  , testCase "Passing parameter 2" $
      runSimpleCode 5 (mkUParam #id ()) @?= Right 5
  , testCase "Passing illegal parameter" $
      assertBool "Expected failure" $
      isLeft $
      runSimpleCode 5 (UParamUnsafe ([mt|Nyan|], ""))
  ]
  where
    runSimpleCode initVal uparam = do
      let initStack = (Identity uparam :& initVal :& RNil)
      resStack <- interpretLorentzInstr dummyContractEnv simpleCode initStack
      let Identity res :& RNil = resStack
      return res
