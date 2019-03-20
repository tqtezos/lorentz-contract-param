-- | Utilities for integrational testing.

module Morley.Test.Integrational
  ( IntegrationalValidator
  , SuccessValidator
  , integrationalTestExpectation
  , integrationalTestProperty

  -- * Validators
  , composeValidators
  , expectStorageValue
  , expectStorageConstant
  , expectBalance
  , expectGasExhaustion
  ) where

import Control.Lens (at)
import qualified Data.List as List
import Fmt (blockListF, pretty, (+|), (|+))
import Test.Hspec (Expectation, expectationFailure)
import Test.QuickCheck (Property)

import Michelson.Interpret (InterpretUntypedError(..), MichelsonFailed(..), RemainingSteps)
import Morley.Aliases (UntypedValue)
import Morley.Runtime (InterpreterError(..), InterpreterOp(..), InterpreterRes(..), interpreterPure)
import Morley.Runtime.GState
import Morley.Test.Util (failedProp, succeededProp)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)

-- | Validator for integrational testing.
-- If an error is expected, it should be 'Left' with validator for errors.
-- Otherwise it should check final global state and its updates.
type IntegrationalValidator = Either (InterpreterError -> Bool) SuccessValidator

type SuccessValidator = (GState -> [GStateUpdate] -> Either Text ())

integrationalTestExpectation ::
  Timestamp -> RemainingSteps -> [InterpreterOp] -> IntegrationalValidator -> Expectation
integrationalTestExpectation =
  integrationalTest (maybe pass (expectationFailure . toString))

integrationalTestProperty ::
  Timestamp -> RemainingSteps -> [InterpreterOp] -> IntegrationalValidator -> Property
integrationalTestProperty = integrationalTest (maybe succeededProp failedProp)

integrationalTest ::
     (Maybe Text -> res)
  -> Timestamp
  -> RemainingSteps
  -> [InterpreterOp]
  -> IntegrationalValidator
  -> res
integrationalTest howToFail now maxSteps operations validator =
  validateResult
    howToFail
    validator
    (interpreterPure now maxSteps initGState operations)

validateResult ::
     (Maybe Text -> res)
  -> IntegrationalValidator
  -> Either InterpreterError InterpreterRes
  -> res
validateResult howToFail validator result =
  case (validator, result) of
    (Left validateError, Left err)
      | validateError err -> doNotFail
    (_, Left err) ->
      doFail $ "Unexpected interpreter error: " <> pretty err
    (Left _, Right _) ->
      doFail $ "Interpreter unexpectedly didn't fail"
    (Right validateUpdates, Right ir)
      | Left bad <- validateUpdates (_irGState ir) (_irUpdates ir) ->
        doFail $
        "Updates are incorrect: " +| bad |+ ". Updates are: \n" +|
        blockListF (_irUpdates ir) |+ ""
      | otherwise -> doNotFail
  where
    doNotFail = howToFail Nothing
    doFail = howToFail . Just

----------------------------------------------------------------------------
-- Validators to be used within 'IntegrationalValidator'
----------------------------------------------------------------------------

-- | Check that storage value is updated for given address. Takes a
-- predicate that is used to check the value.
--
-- It works even if updates are not filtered (i. e. a value can be
-- updated more than once).
expectStorageValue ::
     Address
  -> (UntypedValue -> Either Text ())
  -> SuccessValidator
expectStorageValue addr predicate _ updates =
  case List.find checkAddr (reverse updates) of
    Nothing -> Left $ "Storage of " +| addr |+ " is not updated"
    Just (GSSetStorageValue _ val) ->
      first (("Storage of " +| addr |+ "is updated incorrectly: ") <>) $
      predicate val
    -- 'checkAddr' ensures that only 'GSSetStorageValue' can be found
    Just _ -> error "expectStorageValue: internal error"
  where
    checkAddr (GSSetStorageValue addr' _) = addr' == addr
    checkAddr _ = False

-- | Like 'expectStorageValue', but expects a constant.
expectStorageConstant ::
     Address
  -> UntypedValue
  -> SuccessValidator
expectStorageConstant addr expected =
  expectStorageValue addr predicate
  where
    predicate val
      | val == expected = pass
      | otherwise = Left $ "expected " +| expected |+ ""

-- | Check that eventually address has some particular balance.
expectBalance :: Address -> Mutez -> SuccessValidator
expectBalance addr balance gs _ =
  case gsAddresses gs ^. at addr of
    Nothing ->
      Left $
      "Expected " +| addr |+ " to have balance " +| balance |+
      ", but it's unknown"
    Just (asBalance -> realBalance)
      | realBalance == balance -> pass
      | otherwise ->
        Left $
        "Expected " +| addr |+ " to have balance " +| balance |+
        ", but its balance is " +| realBalance |+ ""

-- | Compose two success validators.
--
-- For example:
--
-- expectBalance bal addr `composeValidators`
-- expectStorageConstant addr2 ValueUnit
composeValidators ::
     SuccessValidator
  -> SuccessValidator
  -> SuccessValidator
composeValidators val1 val2 gState updates =
  val1 gState updates >> val2 gState updates

expectGasExhaustion :: InterpreterError -> Bool
expectGasExhaustion =
  \case
    IEInterpreterFailed _ (RuntimeFailure (MichelsonGasExhaustion, _)) -> True
    _ -> False
