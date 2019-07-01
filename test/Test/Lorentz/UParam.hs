{-# LANGUAGE DerivingStrategies #-}

-- | Tests for Lorentz 'UParam'.
module Test.Lorentz.UParam
  ( test_Simple_contract
  , test_ADT_conversion
  ) where

import Data.Vinyl.Core (Rec(..))
import Data.Constraint (Dict (..))
import Test.HUnit ((@?=), assertBool)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Michelson.Text
import Michelson.Interpret.Pack
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

-- Test ADT conversion
----------------------------------------------------------------------------

data Parameter1
  = MyEntryPoint1 Integer
  | MyEntryPoint2 ()
  deriving Generic

type ExpectedLinearization1 =
  [ "MyEntryPoint1" ?: Integer
  , "MyEntryPoint2" ?: ()
  ]

_checkLinearizedType
  :: Dict (UParamLinearized Parameter1 ~ ExpectedLinearization1)
_checkLinearizedType = Dict

test_ADT_conversion :: [TestTree]
test_ADT_conversion =
  [ testCase "Linearization 1.1" $
      uparamFromAdt (MyEntryPoint1 5)
        @?= UParamUnsafe ([mt|MyEntryPoint1|], packValue' (L.toVal @Integer 5))
  , testCase "Linearization 1.2" $
      uparamFromAdt (MyEntryPoint2 ())
        @?= UParamUnsafe ([mt|MyEntryPoint2|], packValue' (L.toVal ()))
  ]
