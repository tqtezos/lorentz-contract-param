-- | Utilities for integrational testing.

module Morley.Test.Integrational
  ( UpdatesValidator
  , integrationalTest
  , expectStorageValue
  , expectStorageConstant
  ) where

import qualified Data.List as List
import Fmt (blockListF, pretty, (+|), (|+))
import Test.Hspec (Expectation, expectationFailure)

import Morley.Aliases (UntypedValue)
import Morley.Runtime (InterpreterError, InterpreterOp(..), InterpreterRes(..), interpreterPure)
import Morley.Runtime.GState
import Tezos.Address (Address)
import Tezos.Core (Timestamp)

-- | Validator for integrational testing.
-- If an error is expected, it should be 'Left' with validator for errors.
-- Otherwise it should check updates of the global state.
type UpdatesValidator =
  Either (InterpreterError -> Bool) ([GStateUpdate] -> Either Text ())

integrationalTest ::
  Timestamp -> Word64 -> [InterpreterOp] -> UpdatesValidator -> Expectation
integrationalTest now maxSteps operations validator =
  validateResult validator (interpreterPure now maxSteps initGState operations)

validateResult ::
     UpdatesValidator
  -> Either InterpreterError InterpreterRes
  -> Expectation
validateResult validator result =
  case (validator, result) of
    (Left validateError, Left err)
      | validateError err -> pass
    (_, Left err) ->
      expectationFailure $ "Unexpected interpreter error: " <> pretty err
    (Left _, Right _) ->
      expectationFailure $ "Interpreter unexpectedly didn't fail"
    (Right validateUpdates, Right (_irUpdates -> updates))
      | Left bad <- validateUpdates updates ->
        expectationFailure $
        "Updates are incorrect: " +| bad |+ ". Updates are: \n" +|
        blockListF updates |+ ""
      | otherwise -> pass

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
