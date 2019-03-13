-- | Utilities for integrational testing.

module Morley.Test.Integrational
  ( UpdatesValidator
  , integrationalTestExpectation
  , integrationalTestProperty
  , expectStorageValue
  , expectStorageConstant
  ) where

import qualified Data.List as List
import Fmt (blockListF, pretty, (+|), (|+))
import Test.Hspec (Expectation, expectationFailure)
import Test.QuickCheck (Property)

import Morley.Aliases (UntypedValue)
import Morley.Runtime (InterpreterError, InterpreterOp(..), InterpreterRes(..), interpreterPure)
import Morley.Runtime.GState
import Morley.Test.Util (failedProp, succeededProp)
import Tezos.Address (Address)
import Tezos.Core (Timestamp)

-- | Validator for integrational testing.
-- If an error is expected, it should be 'Left' with validator for errors.
-- Otherwise it should check updates of the global state.
type UpdatesValidator =
  Either (InterpreterError -> Bool) ([GStateUpdate] -> Either Text ())

integrationalTestExpectation ::
  Timestamp -> Word64 -> [InterpreterOp] -> UpdatesValidator -> Expectation
integrationalTestExpectation =
  integrationalTest (maybe pass (expectationFailure . toString))

integrationalTestProperty ::
  Timestamp -> Word64 -> [InterpreterOp] -> UpdatesValidator -> Property
integrationalTestProperty = integrationalTest (maybe succeededProp failedProp)

integrationalTest ::
     (Maybe Text -> res)
  -> Timestamp
  -> Word64
  -> [InterpreterOp]
  -> UpdatesValidator
  -> res
integrationalTest howToFail now maxSteps operations validator =
  validateResult
    howToFail
    validator
    (interpreterPure now maxSteps initGState operations)

validateResult ::
     (Maybe Text -> res)
  -> UpdatesValidator
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
    (Right validateUpdates, Right (_irUpdates -> updates))
      | Left bad <- validateUpdates updates ->
        doFail $
        "Updates are incorrect: " +| bad |+ ". Updates are: \n" +|
        blockListF updates |+ ""
      | otherwise -> doNotFail
  where
    doNotFail = howToFail Nothing
    doFail = howToFail . Just

----------------------------------------------------------------------------
-- Validators to be used within 'UpdatesValidator'
----------------------------------------------------------------------------

-- | Check that storage value is updated for given address. Takes a
-- predicate that is used to check the value.
--
-- It works even if updates are not filtered (i. e. a value can be
-- updated more than once).
expectStorageValue ::
     [GStateUpdate]
  -> Address
  -> (UntypedValue -> Either Text ())
  -> Either Text ()
expectStorageValue updates addr predicate =
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
     [GStateUpdate]
  -> Address
  -> UntypedValue
  -> Either Text ()
expectStorageConstant updates addr expected =
  expectStorageValue updates addr predicate
  where
    predicate val
      | val == expected = pass
      | otherwise = Left $ "expected " +| expected |+ ""
