-- | Tests for the 'stringCaller.tz' contract.

module Test.Interpreter.StringCaller
  ( stringCallerSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck.Instances.Text ()

import Michelson.Typed
import Michelson.Untyped (UntypedContract)
import qualified Michelson.Untyped as Untyped
import Morley.Runtime.GState
import Morley.Test (specWithContract)
import Morley.Test.Integrational
import Tezos.Address (formatAddress)
import Tezos.Core

stringCallerSpec :: Spec
stringCallerSpec =
  parallel $
  specWithContract "contracts/stringCaller.tz" $ \stringCaller ->
  specWithContract "contracts/idString.tz" $ \idString ->
  specImpl stringCaller idString

specImpl ::
     (UntypedContract, Contract ('Tc 'CString) ('Tc 'CAddress))
  -> (UntypedContract, Contract ('Tc 'CString) ('Tc 'CString))
  -> Spec
specImpl (uStringCaller, _stringCaller) (uIdString, _idString) = do
  let scenario = integrationalScenario uStringCaller uIdString
  it "stringCaller calls idString and updates its storage with a constant" $
    integrationalTestExpectation (scenario constStr)

  -- The test is trivial, so it's kinda useless to run it many times
  modifyMaxSuccess (const 2) $
    prop "stringCaller calls idString and updates its storage with an arbitrary value" $
      \str -> integrationalTestProperty (scenario str)
  where
    constStr = "caller"

integrationalScenario :: UntypedContract -> UntypedContract -> Text -> IntegrationalScenario
integrationalScenario stringCaller idString str = do
  let
    initIdStringBalace = unsafeMkMutez 100
    initStringCallerBalance = unsafeMkMutez 100

  idStringAddress <-
    originate idString (Untyped.ValueString "hello") initIdStringBalace
  stringCallerAddress <-
    originate stringCaller (Untyped.ValueString $ formatAddress idStringAddress)
    initStringCallerBalance

  let
    newValue = Untyped.ValueString str
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = newValue
      , tdAmount = unsafeMkMutez 0
      }

  transfer txData stringCallerAddress

  let
    -- `stringCaller.tz` transfers 2 mutez.
    expectedIdStringBalance = unsafeMkMutez 102
    expectedStringCallerBalance = unsafeMkMutez 98

    updatesValidator :: SuccessValidator
    updatesValidator =
      expectStorageUpdateConst idStringAddress newValue `composeValidators`
      expectBalance idStringAddress expectedIdStringBalance `composeValidators`
      expectBalance stringCallerAddress expectedStringCallerBalance

  validate (Right updatesValidator)
