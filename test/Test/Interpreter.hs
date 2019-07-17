{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

module Test.Interpreter
  ( test_basic5
  , test_increment
  , test_fail
  , test_mutez_add_overflow
  , test_mutez_sub_overflow
  , test_basic1
  , test_lsl
  , test_lsr
  , test_FAILWITH
  , test_STEPS_TO_QUOTA
  , test_gas_exhaustion
  , test_add1_list
  , test_mkStackRef
  , test_Sum_types
  , test_Product_types
  , test_split_bytes
  , test_split_string_simple
  , test_complex_strings
  ) where

import Data.Singletons (SingI)
import Fmt (Buildable, pretty, (+|), (|+))
import Test.Hspec.Expectations (Expectation, expectationFailure, shouldBe, shouldSatisfy)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck (Property, label, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import Lorentz (( # ))
import qualified Lorentz as L
import Michelson.Interpret
  (ContractEnv(..), ContractReturn, MichelsonFailed(..), RemainingSteps, interpret)
import Michelson.Test
  (ContractPropValidator, concatTestTrees, contractProp, testTreesWithTypedContract)
import Michelson.Test.Dummy (dummyContractEnv)
import Michelson.Test.Util (failedProp)
import Michelson.Text
import Michelson.Typed (CT(..), CValue(..), IsoValue(..), T(..))
import qualified Michelson.Typed as T

contractResShouldBe ::
     (Buildable err)
  => (Either err (ops, T.Value storage), is)
  -> T.Value storage
  -> Assertion
contractResShouldBe (res, _) expected =
  case res of
    Left err -> expectationFailure $ "Unexpected failure: " <> pretty err
    Right (_ops, v) -> v @?= expected

test_basic5 :: IO [TestTree]
test_basic5 =
  testTreesWithTypedContract "contracts/basic5.tz" $ \contract -> pure
  [ testCase "Basic test" $
      interpret contract T.VUnit (toVal [1 :: Integer]) dummyContractEnv
        `contractResShouldBe` (toVal [13 :: Integer, 100])
  ]

test_increment :: IO [TestTree]
test_increment =
  testTreesWithTypedContract "contracts/increment.tz" $ \contract -> pure
  [ testCase "Basic test" $
      interpret contract T.VUnit (toVal @Integer 23) dummyContractEnv
        `contractResShouldBe` (toVal @Integer 24)
  ]

test_fail :: IO [TestTree]
test_fail =
  testTreesWithTypedContract "contracts/tezos_examples/fail.tz" $ \contract -> pure
  [ testCase "Fail test" $
      interpret contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)
  ]

test_mutez_add_overflow :: IO [TestTree]
test_mutez_add_overflow =
  testTreesWithTypedContract "contracts/mutez_add_overflow.tz" $ \contract -> pure
  [ testCase "Mutez add overflow test" $
      interpret contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)
  ]

test_mutez_sub_overflow :: IO [TestTree]
test_mutez_sub_overflow =
  testTreesWithTypedContract "contracts/mutez_sub_underflow.tz" $ \contract -> pure
  [ testCase "Mutez sub underflow test" $
      interpret contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)
  ]

test_basic1 :: IO [TestTree]
test_basic1 =
  testTreesWithTypedContract "contracts/basic1.tz" $ \contract -> pure
  [ testProperty "Random check" $ \input ->
      contractProp @_ @[Integer] contract (validateBasic1 input)
      dummyContractEnv () input
  ]

test_lsl :: IO [TestTree]
test_lsl =
  testTreesWithTypedContract "contracts/lsl.tz" $ \contract -> pure
  [ testCase "LSL shouldn't overflow test" $
      interpret contract (toVal @Natural 5) (toVal @Natural 2) dummyContractEnv
        `contractResShouldBe` (toVal @Natural 20)
  , testCase "LSL should overflow test" $
      interpret contract (toVal @Natural 5) (toVal @Natural 257) dummyContractEnv
        `shouldSatisfy` (isLeft . fst)
  ]

test_lsr :: IO [TestTree]
test_lsr =
  testTreesWithTypedContract "contracts/lsr.tz" $ \contract -> pure
  [ testCase "LSR shouldn't underflow test" $
      interpret contract (toVal @Natural 30) (toVal @Natural 3) dummyContractEnv
        `contractResShouldBe` (toVal @Natural 3)
  , testCase "LSR should underflow test" $
      interpret contract (toVal @Natural 1000) (toVal @Natural 257) dummyContractEnv
        `shouldSatisfy` (isLeft . fst)
  ]

test_FAILWITH :: IO [TestTree]
test_FAILWITH = concatTestTrees
  [ testTreesWithTypedContract "contracts/failwith_message.tz" $ \contract ->
    pure
    [ testCase "Failwith message test" $ do
        let msg = [mt|An error occurred.|] :: MText
        contractProp contract (validateMichelsonFailsWith msg) dummyContractEnv
          msg ()
    ]
  , testTreesWithTypedContract "contracts/failwith_message2.tz" $ \contract ->
    pure
    [ testCase "Conditional failwith message test" $ do
        let msg = [mt|An error occurred.|]
        contractProp contract (validateMichelsonFailsWith msg) dummyContractEnv
          (True, msg) ()

    , testCase "Conditional success test" $ do
        let param = (False, [mt|Err|] :: MText)
        contractProp contract validateSuccess dummyContractEnv param ()
    ]
  ]

test_STEPS_TO_QUOTA :: IO [TestTree]
test_STEPS_TO_QUOTA = concatTestTrees
  [ testTreesWithTypedContract "contracts/steps_to_quota_test1.tz" $ \contract ->
    pure
    [ testCase "Amount of steps should decrease (1)" $ do
      validateStepsToQuotaTest
        (interpret contract T.VUnit (T.VC (CvNat 0)) dummyContractEnv) 4
    ]
  , testTreesWithTypedContract "contracts/steps_to_quota_test2.tz" $ \contract ->
    pure
    [ testCase "Amount of steps should decrease (2)" $ do
      validateStepsToQuotaTest
        (interpret contract T.VUnit (T.VC (CvNat 0)) dummyContractEnv) 8
    ]
  ]

test_gas_exhaustion :: IO [TestTree]
test_gas_exhaustion =
  testTreesWithTypedContract "contracts/gas_exhaustion.tz" $ \contract -> pure
  [ testCase "Contract should fail due to gas exhaustion" $ do
      let dummyStr = toVal [mt|x|]
      case fst $ interpret contract dummyStr dummyStr dummyContractEnv of
        Right _ -> assertFailure "expecting contract to fail"
        Left MichelsonGasExhaustion -> pass
        Left _ -> assertFailure "expecting another failure reason"
  ]

test_add1_list :: IO [TestTree]
test_add1_list =
  testTreesWithTypedContract "contracts/tezos_examples/add1_list.tz" $ \contract ->
  let
    validate ::
      [Integer] -> ContractPropValidator (ToT [Integer]) Property
    validate param (res, _) =
      case res of
        Left failed -> failedProp $
          "add1_list unexpectedly failed: " <> pretty failed
        Right (fromVal . snd -> finalStorage) ->
          map succ param === finalStorage
  in pure
  [ testProperty "Random check" $ \param ->
      contractProp contract (validate param) dummyContractEnv param param
  ]

test_mkStackRef :: TestTree
test_mkStackRef =
  testCase "does not segfault" $ do
    let contract = L.drop # L.push () # L.dup # L.printComment (L.stackRef @1)
                          # L.drop # L.nil @T.Operation # L.pair
    contractProp (L.compileLorentzContract @() contract)
      (flip shouldSatisfy isRight . fst) dummyContractEnv () ()

test_Sum_types :: IO [TestTree]
test_Sum_types = concatTestTrees
  [ testTreesWithTypedContract "contracts/union.mtz" $ \contract -> pure
    [ testGroup "union.mtz: union corresponds to Haskell types properly" $
        let caseTest param =
              contractProp contract validateSuccess dummyContractEnv param ()
        in
        [ testCase "Case 1" $ caseTest (Case1 3)
        , testCase "Case 2" $ caseTest (Case2 [mt|a|])
        , testCase "Case 3" $ caseTest (Case3 $ Just [mt|b|])
        , testCase "Case 4" $ caseTest (Case4 $ Left [mt|b|])
        , testCase "Case 5" $ caseTest (Case5 [[mt|q|]])
        ]
    ]
  , testTreesWithTypedContract "contracts/case.mtz" $ \contract -> pure
    [ testGroup "CASE instruction" $
        let caseTest param expectedStorage =
              contractProp contract (validateStorageIs @MText expectedStorage)
                dummyContractEnv param [mt||]
        in
        [ testCase "Case 1" $ caseTest (Case1 5) [mt|int|]
        , testCase "Case 2" $ caseTest (Case2 [mt|a|]) [mt|string|]
        , testCase "Case 3" $ caseTest (Case3 $ Just [mt|aa|]) [mt|aa|]
        , testCase "Case 4" $ caseTest (Case4 $ Right [mt|b|]) [mt|or string string|]
        , testCase "Case 5" $ caseTest (Case5 $ [[mt|a|], [mt|b|]]) [mt|ab|]
        ]
    ]
  , testTreesWithTypedContract "contracts/tag.mtz" $ \contract -> pure
    [ testCase "TAG instruction" $
        let expected = mconcat [[mt|unit|], [mt|o|], [mt|ab|], [mt|nat|], [mt|int|]]
        in contractProp contract (validateStorageIs expected) dummyContractEnv () [mt||]
    ]
  ]

test_Product_types :: IO [TestTree]
test_Product_types = concatTestTrees
  [ testTreesWithTypedContract "contracts/access.mtz" $ \contract -> pure
    [ testCase "ACCESS instruction" $
        contractProp @Tuple1 contract validateSuccess dummyContractEnv
          (1, [mt|a|], Just [mt|a|], Right [mt|a|], [[mt|a|]]) ()
    ]
  , testTreesWithTypedContract "contracts/set.mtz" $ \contract -> pure
    [ testCase "SET instruction" $
      let expected = (2, [mt|za|], Just [mt|wa|], Right [mt|ya|], [[mt|ab|]]) :: Tuple1
      in contractProp @_ @Tuple1 contract (validateStorageIs expected)
        dummyContractEnv () (1, [mt|a|], Just [mt|a|], Right [mt|a|], [[mt|a|], [mt|b|]])
    ]
  , testTreesWithTypedContract "contracts/construct.mtz" $ \contract -> pure
    [ testCase "CONSTRUCT instruction" $
      let expected = (1, [mt|a|], Just [mt|b|], Left [mt|q|], []) :: Tuple1
      in contractProp @_ @Tuple1 contract (validateStorageIs expected)
        dummyContractEnv () (0, [mt||], Nothing, Right [mt||], [])
    ]
  ]

test_split_bytes :: IO [TestTree]
test_split_bytes =
  testTreesWithTypedContract "contracts/tezos_examples/split_bytes.tz" $ \contract ->
  pure
  [ testCase "splits given byte sequence into parts" $
      let expected = ["\11", "\12", "\13"] :: [ByteString]
      in contractProp contract (validateStorageIs expected) dummyContractEnv
         ("\11\12\13" :: ByteString) ([] :: [ByteString])
  ]

test_split_string_simple :: IO [TestTree]
test_split_string_simple =
  testTreesWithTypedContract "contracts/split_string_simple.tz" $ \contract ->
  pure
  [ testCase "applies SLICE instruction" $ do
      let
        oneTest :: Natural -> Natural -> MText -> Maybe MText -> Expectation
        oneTest o l str expected =
          contractProp contract (validateStorageIs expected) dummyContractEnv
          (o, l) (Just str)

      -- These values have been tested using alphanet.sh
      oneTest 0 0 [mt|aaa|] (Just [mt||])
      oneTest 2 0 [mt|aaa|] (Just [mt||])
      oneTest 3 0 [mt|aaa|] Nothing
      oneTest 0 5 [mt|aaa|] Nothing
      oneTest 1 2 [mt|abc|] (Just [mt|bc|])
      oneTest 1 1 [mt|abc|] (Just [mt|b|])
      oneTest 2 1 [mt|abc|] (Just [mt|c|])
      oneTest 2 2 [mt|abc|] Nothing
      oneTest 1 1 [mt|a""|] (Just [mt|"|])
      oneTest 1 2 [mt|a\n|] Nothing
  ]

test_complex_strings :: IO [TestTree]
test_complex_strings =
  testTreesWithTypedContract "contracts/complex_strings.tz" $ \contract ->
  pure
  [ testCase "ComplexString" $
      contractProp contract
      (validateStorageIs [mt|text: "aa" \\\n|])
      dummyContractEnv [mt|text: |] [mt||]
  ]

data Union1
  = Case1 Integer
  | Case2 MText
  | Case3 (Maybe MText)
  | Case4 (Either MText MText)
  | Case5 [MText]
  deriving stock (Generic)
  deriving anyclass (IsoValue)

type Tuple1 = (Integer, MText, Maybe MText, Either MText MText, [MText])

validateSuccess :: ContractPropValidator st Expectation
validateSuccess (res, _) = res `shouldSatisfy` isRight

validateStorageIs
  :: IsoValue st
  => st -> ContractPropValidator (ToT st) Expectation
validateStorageIs expected (res, _) =
  case res of
    Left err ->
      expectationFailure $ "Unexpected interpretation failure: " +| err |+ ""
    Right (_ops, got) ->
      got `shouldBe` toVal expected

validateBasic1
  :: [Integer] -> ContractPropValidator ('TList ('Tc 'CInt)) Property
validateBasic1 input (Right (ops, res), _) =
    (fromVal res === [sum input + 12, 100])
    .&&.
    (label "returned no ops" $ null ops)
validateBasic1 _ (Left e, _) = failedProp $ show e

validateStepsToQuotaTest ::
  ContractReturn ('Tc 'CNat) -> RemainingSteps -> Expectation
validateStepsToQuotaTest res numOfSteps =
  case fst res of
    Right ([], T.VC (CvNat x)) ->
      (fromInteger . toInteger) x `shouldBe` ceMaxSteps dummyContractEnv - numOfSteps
    _ -> expectationFailure "unexpected contract result"

validateMichelsonFailsWith
  :: (T.IsoValue v, Typeable (ToT v), SingI (ToT v))
  => v -> ContractPropValidator st Expectation
validateMichelsonFailsWith v (res, _) = res `shouldBe` Left (MichelsonFailedWith $ toVal v)
