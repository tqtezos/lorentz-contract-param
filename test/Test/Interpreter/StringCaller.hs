-- | Tests for the 'stringCaller.tz' contract and its interaction with
-- the 'failOrStoreAndTransfer.tz' contract. Both of them have comments describing
-- their behavior.

module Test.Interpreter.StringCaller
  ( stringCallerSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck.Instances.Text ()

import Michelson.Runtime.GState
import Michelson.Test (specWithContract)
import Michelson.Test.Integrational
import Michelson.Typed
import Michelson.Text
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Tezos.Address
import Tezos.Core

stringCallerSpec :: Spec
stringCallerSpec =
  parallel $
  specWithContract "contracts/stringCaller.tz" $ \stringCaller ->
  specWithContract "contracts/failOrStoreAndTransfer.tz" $ \failOrStoreAndTransfer ->
  specImpl stringCaller failOrStoreAndTransfer

specImpl ::
     (U.Contract, T.Contract ('Tc 'CString) ('Tc 'CAddress))
  -> (U.Contract, T.Contract ('Tc 'CString) ('Tc 'CString))
  -> Spec
specImpl (uStringCaller, _stringCaller) (uFailOrStore, _failOrStoreAndTransfer) = do
  let scenario = integrationalScenario uStringCaller uFailOrStore
  let prefix =
        "stringCaller calls failOrStoreAndTransfer and updates its storage with "
  let suffix =
        " and properly updates balances. But fails if failOrStoreAndTransfer's"
        <> " balance is ≥ 1000 and NOW is ≥ 500"
  it (prefix <> "a constant" <> suffix) $
    integrationalTestExpectation (scenario constStr)

  -- The test is trivial, so it's kinda useless to run it many times
  modifyMaxSuccess (const 2) $
    prop (prefix <> "an arbitrary value" <> suffix) $
      \str -> integrationalTestProperty (scenario str)
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
  setNow (timestampFromSeconds @Int 500)

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
  setNow (timestampFromSeconds @Int 600)
  transferToStringCaller
  validate (Left $ expectMichelsonFailed (const True) stringCallerAddress)

-- Address hardcoded in 'failOrStoreAndTransfer.tz'.
constAddr :: Address
constAddr = unsafeParseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
