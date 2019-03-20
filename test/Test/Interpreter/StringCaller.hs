-- | Tests for the 'stringCaller.tz' contract.

module Test.Interpreter.StringCaller
  ( stringCallerSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (arbitrary, forAll)

import Michelson.Typed
import Michelson.Untyped (OriginationOperation(..), mkContractAddress)
import qualified Michelson.Untyped as Untyped
import Morley.Aliases (UntypedContract, UntypedValue)
import Morley.Runtime (InterpreterOp(..), TxData(..))
import Morley.Runtime.GState
import Morley.Test (specWithContract)
import Morley.Test.Integrational
  (SuccessValidator, composeValidators, expectBalance, expectStorageConstant)
import Tezos.Address (formatAddress)
import Tezos.Core

import Test.Arbitrary ()
import Test.Util.Interpreter

stringCallerSpec :: Spec
stringCallerSpec =
  parallel $
  specWithContract "contracts/stringCaller.tz" $ \stringCaller ->
  specWithContract "contracts/idString.tz" $ \idString ->
  specImpl stringCaller idString

specImpl ::
     (UntypedContract, Contract ('T_c 'T_string) ('T_c 'T_address))
  -> (UntypedContract, Contract ('T_c 'T_string) ('T_c 'T_string))
  -> Spec
specImpl (uStringCaller, _stringCaller) (uIdString, _idString) = do
  it "stringCaller calls idString and updates its storage with a constant" $
    simplerIntegrationalTestExpectation
      (operations newValueConstant)
      (Right (updatesValidator newValueConstant))

  -- The test is trivial, so it's kinda useless to run it many times
  modifyMaxSuccess (const 2) $
    prop "stringCaller calls idString and updates its storage with an arbitrary value" $
    forAll arbitrary $ \(Untyped.ValueString -> newValue) ->
      simplerIntegrationalTestProperty
        (operations newValue)
        (Right (updatesValidator newValue))
  where
    newValueConstant = Untyped.ValueString "caller"

    idStringOrigination :: OriginationOperation
    idStringOrigination =
      dummyOrigination (Untyped.ValueString "hello") uIdString
    originateIdString = OriginateOp idStringOrigination
    idStringAddress = mkContractAddress idStringOrigination

    stringCallerOrigination :: OriginationOperation
    stringCallerOrigination =
      dummyOrigination (Untyped.ValueString $ formatAddress idStringAddress)
      uStringCaller
    originateStringCaller = OriginateOp stringCallerOrigination
    stringCallerAddress = mkContractAddress stringCallerOrigination

    txData :: UntypedValue -> TxData
    txData newValue = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = newValue
      , tdAmount = minBound
      }
    transferToStringCaller newValue =
      TransferOp stringCallerAddress (txData newValue)

    operations newValue =
      [ originateIdString
      , originateStringCaller
      , transferToStringCaller newValue
      ]

    -- `stringCaller.tz` transfers 2 mutez.
    expectedIdStringBalance =
      ooBalance idStringOrigination `unsafeAddMutez` unsafeMkMutez 2

    updatesValidator :: UntypedValue -> SuccessValidator
    updatesValidator newValue =
      expectStorageConstant idStringAddress newValue `composeValidators`
      expectBalance idStringAddress expectedIdStringBalance
