-- | Tests for the 'stringCaller.tz' contract.

module Test.Interpreter.StringCaller
  ( stringCallerSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck.Instances.Text ()

import Michelson.Typed
import Michelson.Untyped (OriginationOperation(..), mkContractAddress)
import qualified Michelson.Untyped as Untyped
import Morley.Aliases (UntypedContract, UntypedValue)
import Morley.Runtime (InterpreterOp(..), TxData(..))
import Morley.Runtime.GState
import Morley.Test (specWithContract)
import Morley.Test.Dummy
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
  it "stringCaller calls idString and updates its storage with a constant" $
    simplerIntegrationalTestExpectation
      (operations newValueConstant)
      (Right (updatesValidator newValueConstant))

  -- The test is trivial, so it's kinda useless to run it many times
  modifyMaxSuccess (const 2) $
    prop "stringCaller calls idString and updates its storage with an arbitrary value" $
      \(Untyped.ValueString -> newValue) -> simplerIntegrationalTestProperty
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