#!/usr/bin/env stack
{- stack
  script
  --resolver snapshot.yaml
  --package base
  --package text
  --package fmt
  --package hspec
  --package QuickCheck
  --package quickcheck-instances
  --package morley
-}

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module StringCallerSpec where

import Data.Text (Text)
import Test.Hspec (Spec, hspec, it, parallel)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck.Instances.Text ()

import Michelson.Test
  (IntegrationalScenario, SuccessValidator, TxData(..), composeValidatorsList, expectAnySuccess,
  expectBalance, expectMichelsonFailed, expectStorageUpdateConst, genesisAddress,
  integrationalTestExpectation, integrationalTestProperty, originate, setNow, specWithContract,
  transfer, validate)
import Michelson.Text (MText, mt)
import Michelson.Typed (Contract, ToT)
import qualified Michelson.Untyped as Untyped
import Tezos.Address (Address, mformatAddress, unsafeParseAddress)
import Tezos.Core (timestampFromSeconds, unsafeMkMutez)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  parallel $
  specWithContract "contracts/stringCaller.tz" $ \stringCaller ->
  specWithContract "contracts/failOrStoreAndTransfer.tz" $ \failOrStoreAndTransfer ->
  specImpl stringCaller failOrStoreAndTransfer

specImpl ::
     (Untyped.Contract, Contract (ToT MText) (ToT Address))
  -> (Untyped.Contract, Contract (ToT MText) (ToT MText))
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

integrationalScenario :: Untyped.Contract -> Untyped.Contract -> MText -> IntegrationalScenario
integrationalScenario stringCaller failOrStoreAndTransfer str = do
  let
    initFailOrStoreBalance = unsafeMkMutez 900
    initStringCallerBalance = unsafeMkMutez 500

  -- Originate both contracts
  failOrStoreAndTransferAddress <-
    originate failOrStoreAndTransfer "failOrStoreAndTransfer" (Untyped.ValueString [mt|hello|]) initFailOrStoreBalance
  stringCallerAddress <-
    originate stringCaller "stringCaller"
    (Untyped.ValueString $ mformatAddress failOrStoreAndTransferAddress)
    initStringCallerBalance

  -- NOW = 500, so stringCaller shouldn't fail
  setNow (timestampFromSeconds (500 :: Int))

  -- Transfer 100 tokens to stringCaller, it should transfer 300 tokens
  -- to failOrStoreAndTransfer
  let
    newValue = Untyped.ValueString str
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
  validate (Left $ expectMichelsonFailed (const True) failOrStoreAndTransferAddress)

  -- We can also send tokens from failOrStoreAndTransfer to tz1 address directly
  let
    txDataToConst = TxData
      { tdSenderAddress = failOrStoreAndTransferAddress
      , tdParameter = Untyped.ValueUnit
      , tdAmount = unsafeMkMutez 200
      }
  transfer txDataToConst constAddr

  -- Let's check balance of failOrStoreAndTransfer and tz1 address
  do
    let
      expectedFailOrStoreBalance = unsafeMkMutez (900 + 300 - 5 - 200)
      expectedConstAddrBalance = unsafeMkMutez (5 + 200)

      updatesValidator :: SuccessValidator
      updatesValidator = composeValidatorsList
        [ expectBalance failOrStoreAndTransferAddress expectedFailOrStoreBalance
        , expectBalance constAddr expectedConstAddrBalance
        ]

    validate (Right updatesValidator)

  -- Now we can transfer to stringCaller again and it should succeed
  -- this time, because the balance of failOrStoreAndTransfer decreased
  transferToStringCaller

  -- Let's simply assert that it should succeed to keep the scenario shorter
  validate (Right expectAnySuccess)

  -- Now let's set NOW to 600 and expect stringCaller to fail
  setNow (timestampFromSeconds (600 :: Int))
  transferToStringCaller
  validate (Left $ expectMichelsonFailed (const True) stringCallerAddress)

-- Address hardcoded in 'failOrStoreAndTransfer.tz'.
constAddr :: Address
constAddr = unsafeParseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
