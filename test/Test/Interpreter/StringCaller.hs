-- | Tests for the 'stringCaller.tz' contract and its interaction with
-- the 'failOrStoreAndTransfer.tz' contract. Both of them have comments describing
-- their behavior.

module Test.Interpreter.StringCaller
  ( test_stringCaller
  ) where

import Test.QuickCheck (withMaxSuccess)
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)

import Michelson.Runtime.GState
import Michelson.Test (testTreesWithContract)
import Michelson.Test.Integrational
import Michelson.Text
import Michelson.Typed
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Tezos.Address
import Tezos.Core

test_stringCaller :: IO [TestTree]
test_stringCaller =
  testTreesWithContract "contracts/string_caller.tz" $ \stringCaller ->
  testTreesWithContract "contracts/fail_or_store_and_transfer.tz" $ \failOrStoreAndTransfer ->
  pure $ testImpl stringCaller failOrStoreAndTransfer

testImpl ::
     (U.Contract, T.Contract ('Tc 'CString) ('Tc 'CAddress))
  -> (U.Contract, T.Contract ('Tc 'CString) ('Tc 'CString))
  -> [TestTree]
testImpl (uStringCaller, _stringCaller) (uFailOrStore, _failOrStoreAndTransfer) =
  let scenario = integrationalScenario uStringCaller uFailOrStore
      prefix =
        "stringCaller calls failOrStoreAndTransfer and updates its storage with "
      suffix =
        " and properly updates balances. But fails if failOrStoreAndTransfer's"
        <> " balance is ≥ 1000 and NOW is ≥ 500"
  in
  [ testCase (prefix <> "a constant" <> suffix) $
    integrationalTestExpectation (scenario constStr)
  -- The test is trivial, so it's kinda useless to run it many times
  , testProperty (prefix <> "an arbitrary value" <> suffix) $
    withMaxSuccess 2 $
      \str -> integrationalTestProperty (scenario str)
  ]
  where
    constStr = [mt|caller|]

integrationalScenario :: U.Contract -> U.Contract -> MText -> IntegrationalScenario
integrationalScenario stringCaller failOrStoreAndTransfer str = do
  let
    initFailOrStoreBalance = unsafeMkMutez 900
    initStringCallerBalance = unsafeMkMutez 500

  -- Originate both contracts
  failOrStoreAndTransferAddress <-
    originate failOrStoreAndTransfer "failOrStoreAndTransfer" (U.ValueString [mt|hello|]) initFailOrStoreBalance
  stringCallerAddress <-
    originate stringCaller "stringCaller"
    (U.ValueString $ mformatAddress failOrStoreAndTransferAddress)
    initStringCallerBalance

  -- NOW = 500, so stringCaller shouldn't fail
  setNow (timestampFromSeconds 500)

  -- Transfer 100 tokens to stringCaller, it should transfer 300 tokens
  -- to failOrStoreAndTransfer
  let
    newValue = untypeValue $ toVal str
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = newValue
      , tdAmount = unsafeMkMutez 100
      }
    transferToStringCaller = transfer txData stringCallerAddress
  transferToStringCaller

  -- Execute operations and check balances and storage of 'failOrStoreAndTransfer'
  do
    let
      -- `stringCaller.tz` transfers 300 mutez.
      -- 'failOrStoreAndTransfer.tz' transfers 5 tokens.
      -- Also 100 tokens are transferred from the genesis address.
      expectedStringCallerBalance = unsafeMkMutez (500 - 300 + 100)
      expectedFailOrStoreBalance = unsafeMkMutez (900 + 300 - 5)
      expectedConstAddrBalance = unsafeMkMutez 5

      updatesValidator :: SuccessValidator
      updatesValidator = composeValidatorsList
        [ expectStorageUpdateConst failOrStoreAndTransferAddress newValue
        , expectBalance failOrStoreAndTransferAddress expectedFailOrStoreBalance
        , expectBalance stringCallerAddress expectedStringCallerBalance
        , expectBalance constAddr expectedConstAddrBalance
        ]
    validate (Right updatesValidator)

  -- Now let's transfer 100 tokens to stringCaller again.
  transferToStringCaller

  -- This time execution should fail, because failOrStoreAndTransfer should fail
  -- because its balance is greater than 1000.
  void $ validate (Left $ expectMichelsonFailed (const True) failOrStoreAndTransferAddress)

  -- We can also send tokens from failOrStoreAndTransfer to tz1 address directly
  let
    txDataToConst = TxData
      { tdSenderAddress = failOrStoreAndTransferAddress
      , tdParameter = U.ValueUnit
      , tdAmount = unsafeMkMutez 200
      }
  transfer txDataToConst constAddr

  -- Let's check balance of failOrStoreAndTransfer and tz1 address.
  -- We transferred 200 tokens from failOrStoreAndTransferAddress to constAddr.
  do
    let
      expectedFailOrStoreBalance = unsafeMkMutez (900 + 300 - 5 - 200)
      expectedConstAddrBalance = unsafeMkMutez (5 + 200)

      updatesValidator :: SuccessValidator
      updatesValidator = composeValidatorsList
        [ expectBalance failOrStoreAndTransferAddress expectedFailOrStoreBalance
        , expectBalance constAddr expectedConstAddrBalance
        ]

    void $ validate (Right updatesValidator)

  -- Now we can transfer to stringCaller again and it should succeed
  -- this time, because the balance of failOrStoreAndTransfer decreased
  transferToStringCaller

  -- Let's simply assert that it should succeed to keep the scenario shorter
  void $ validate (Right expectAnySuccess)

  -- Now let's set NOW to 600 and expect stringCaller to fail
  setNow (timestampFromSeconds 600)
  transferToStringCaller
  validate (Left $ expectMichelsonFailed (const True) stringCallerAddress)

-- Address hardcoded in 'failOrStoreAndTransfer.tz'.
constAddr :: Address
constAddr = unsafeParseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
