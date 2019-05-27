-- | Utilities for integrational testing.
-- Example tests can be found in the 'morley-test' test suite.

module Michelson.Test.Integrational
  (
    -- * Re-exports
    TxData (..)
  , genesisAddress

  -- * Testing engine
  , IntegrationalValidator
  , SuccessValidator
  , IntegrationalScenarioM
  , IntegrationalScenario
  , ValidationError (..)
  , integrationalTestExpectation
  , integrationalTestProperty
  , originate
  , transfer
  , validate
  , setMaxSteps
  , setNow
  , withSource

  -- * Validators
  , composeValidators
  , composeValidatorsList
  , expectAnySuccess
  , expectStorageUpdate
  , expectStorageUpdateConst
  , expectBalance
  , expectStorageConst
  , expectGasExhaustion
  , expectMichelsonFailed
  ) where

import Control.Lens (assign, at, makeLenses, (%=), (.=), (<>=), (?=))
import Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.List as List
import Data.Map as Map (empty, insert, lookup)
import Fmt (Buildable(..), blockListF, pretty, (+|), (|+))
import Test.Hspec (Expectation, expectationFailure)
import Test.QuickCheck (Property)

import Michelson.Interpret (InterpretUntypedError(..), MichelsonFailed(..), RemainingSteps)
import Michelson.Runtime
  (InterpreterError, InterpreterError'(..), InterpreterOp(..), InterpreterRes(..), interpreterPure)
import Michelson.Runtime.GState
import Michelson.Runtime.TxData
import Michelson.Test.Dummy
import Michelson.Test.Util (failedProp, succeededProp)
import Michelson.TypeCheck (TCError)
import Michelson.Untyped (Contract, OriginationOperation(..), Value, mkContractAddress)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)

----------------------------------------------------------------------------
-- Some internals (they are here because TH makes our very existence much harder)
----------------------------------------------------------------------------

data InternalState = InternalState
  { _isMaxSteps :: !RemainingSteps
  , _isNow :: !Timestamp
  , _isGState :: !GState
  , _isOperations :: ![InterpreterOp]
  -- ^ Operations to be interpreted when 'TOValidate' is encountered.
  , _isContractsNames :: !(Map Address Text)
  -- ^ Map from contracts addresses to humanreadable names.
  , _isSource :: !(Maybe Address)
  -- ^ If set, all following transfers will be executed on behalf
  -- of the given contract.
  }

makeLenses ''InternalState

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- | Validator for integrational testing.
-- If an error is expected, it should be 'Left' with validator for errors.
-- Otherwise it should check final global state and its updates.
type IntegrationalValidator = Either (InterpreterError -> Bool) SuccessValidator

-- | Validator for integrational testing that expects successful execution.
type SuccessValidator = (InternalState -> GState -> [GStateUpdate] -> Either ValidationError ())

-- | A monad inside which integrational tests can be described using
-- do-notation.
type IntegrationalScenarioM = StateT InternalState (Except ValidationError)

-- | A dummy data type that ensures that `validate` is called in the
-- end of each scenario. It is intentionally not exported.
data Validated = Validated

type IntegrationalScenario = IntegrationalScenarioM Validated

newtype ExpectedStorage = ExpectedStorage Value deriving (Show)
newtype ExpectedBalance = ExpectedBalance Mutez deriving (Show)

data AddressName = AddressName (Maybe Text) Address deriving (Show)

addrToAddrName :: Address -> InternalState -> AddressName
addrToAddrName addr iState =
  AddressName (lookup addr (iState ^. isContractsNames)) addr

instance Buildable AddressName where
  build (AddressName mbName addr) =
    build addr +| maybe "" (\cName -> " (" +|cName |+ ")") mbName

type IntegrationalInterpreterError = InterpreterError' AddressName

data ValidationError
  = UnexpectedInterpreterError IntegrationalInterpreterError
  | UnexpectedTypeCheckError TCError
  | ExpectingInterpreterToFail
  | IncorrectUpdates ValidationError [GStateUpdate]
  | IncorrectStorageUpdate AddressName Text
  | InvalidStorage AddressName ExpectedStorage Text
  | InvalidBalance AddressName ExpectedBalance Text
  | CustomError Text
  deriving (Show)

instance Buildable ValidationError where
  build (UnexpectedInterpreterError iErr) =
    "Unexpected interpreter error. Reason: " +| iErr |+ ""
  build (UnexpectedTypeCheckError tcErr) =
    "Unexpected type check error. Reason: " +| tcErr |+ ""
  build ExpectingInterpreterToFail =
    "Interpreter unexpectedly didn't fail"
  build (IncorrectUpdates vErr updates) =
    "Updates are incorrect: " +| vErr |+ " . Updates are:"
    +| blockListF updates |+ ""
  build (IncorrectStorageUpdate addr msg) =
    "Storage of " +| addr |+ "is updated incorrectly: " +| msg |+ ""
  build (InvalidStorage addr (ExpectedStorage expected) msg) =
    "Expected " +| addr |+ " to have storage " +| expected |+ ", but " +| msg |+ ""
  build (InvalidBalance addr (ExpectedBalance expected) msg) =
    "Expected " +| addr |+ " to have balance " +| expected |+ ", but " +| msg |+ ""
  build (CustomError msg) = pretty msg

instance Exception ValidationError where
  displayException = pretty

-- | Integrational test that executes given operations and validates
-- them using given validator. It can fail using 'Expectation'
-- capability.
-- It starts with 'initGState' and some reasonable dummy values for
-- gas limit and current timestamp. You can update blockchain state
-- by performing some operations.
integrationalTestExpectation :: IntegrationalScenario -> Expectation
integrationalTestExpectation =
  integrationalTest (maybe pass (expectationFailure . pretty))

-- | Integrational test similar to 'integrationalTestExpectation'.
-- It can fail using 'Property' capability.
-- It can be used with QuickCheck's @forAll@ to make a
-- property-based test with arbitrary data.
integrationalTestProperty :: IntegrationalScenario -> Property
integrationalTestProperty =
  integrationalTest (maybe succeededProp (failedProp . pretty))

-- | Originate a contract with given initial storage and balance. Its
-- address is returned.
originate :: Contract -> Text -> Value -> Mutez -> IntegrationalScenarioM Address
originate contract contractName value balance = do
  address <- mkContractAddress origination <$ putOperation originateOp
  isContractsNames %= (insert address contractName)
  pure address
  where
    origination = (dummyOrigination value contract) {ooBalance = balance}
    originateOp = OriginateOp origination

-- | Transfer tokens to given address.
transfer :: TxData -> Address -> IntegrationalScenarioM ()
transfer txData destination = do
  mSource <- use isSource
  putOperation (TransferOp destination txData mSource)

-- | Execute all operations that were added to the scenarion since
-- last 'validate' call. If validator fails, the execution will be aborted.
validate :: IntegrationalValidator -> IntegrationalScenario
validate validator = Validated <$ do
  now <- use isNow
  maxSteps <- use isMaxSteps
  gState <- use isGState
  ops <- use isOperations
  iState <- get
  let interpret = interpreterPure now maxSteps gState ops
  mUpdatedGState <- lift $ validateResult validator interpret iState
  isOperations .= mempty
  whenJust mUpdatedGState $ \newGState -> isGState .= newGState

-- | Make all further interpreter calls (which are triggered by the
-- 'validate' function) use given timestamp as the current one.
setNow :: Timestamp -> IntegrationalScenarioM ()
setNow = assign isNow

-- | Make all further interpreter calls (which are triggered by the
-- 'validate' function) use given gas limit.
setMaxSteps :: RemainingSteps -> IntegrationalScenarioM ()
setMaxSteps = assign isMaxSteps

-- | Pretend that given address initiates all the transfers within the
-- code block (i.e. @SOURCE@ instruction will return this address).
withSource :: Address -> IntegrationalScenarioM a -> IntegrationalScenarioM a
withSource addr scenario =
  (isSource ?= addr) *> scenario <* (isSource .= Nothing)

putOperation :: InterpreterOp -> IntegrationalScenarioM ()
putOperation op = isOperations <>= one op

----------------------------------------------------------------------------
-- Validators to be used within 'IntegrationalValidator'
----------------------------------------------------------------------------

-- | 'SuccessValidator' that always passes.
expectAnySuccess :: SuccessValidator
expectAnySuccess _ _ _ = pass

-- | Check that storage value is updated for given address. Takes a
-- predicate that is used to check the value.
--
-- It works even if updates are not filtered (i. e. a value can be
-- updated more than once).
expectStorageUpdate ::
     Address
  -> (Value -> Either ValidationError ())
  -> SuccessValidator
expectStorageUpdate addr predicate is _ updates =
  case List.find checkAddr (reverse updates) of
    Nothing -> Left $
      IncorrectStorageUpdate (addrToAddrName addr is) "storage wasn't updated"
    Just (GSSetStorageValue _ val) ->
      first (IncorrectStorageUpdate (addrToAddrName addr is) . pretty) $
      predicate val
    -- 'checkAddr' ensures that only 'GSSetStorageValue' can be found
    Just _ -> error "expectStorageUpdate: internal error"
  where
    checkAddr (GSSetStorageValue addr' _) = addr' == addr
    checkAddr _ = False

-- | Like 'expectStorageUpdate', but expects a constant.
expectStorageUpdateConst ::
     Address
  -> Value
  -> SuccessValidator
expectStorageUpdateConst addr expected is =
  expectStorageUpdate addr predicate is
  where
    predicate val
      | val == expected = pass
      | otherwise = Left $
        IncorrectStorageUpdate (addrToAddrName addr is) $ pretty expected

-- | Check that eventually address has some particular storage value.
expectStorageConst :: Address -> Value -> SuccessValidator
expectStorageConst addr expected is gs _ =
  case gsAddresses gs ^. at addr of
    Just (ASContract cs)
      | csStorage cs == expected -> pass
      | otherwise ->
        Left $ intro $ "its actual storage is: " <> (pretty $ csStorage cs)
    Just (ASSimple {}) ->
      Left $ intro $ "it's a simple address"
    Nothing -> Left $ intro $ "it's unknown"
  where
    intro = InvalidStorage (addrToAddrName addr is) (ExpectedStorage expected)

-- | Check that eventually address has some particular balance.
expectBalance :: Address -> Mutez -> SuccessValidator
expectBalance addr balance is gs _ =
  case gsAddresses gs ^. at addr of
    Nothing ->
      Left $
      InvalidBalance (addrToAddrName addr is) (ExpectedBalance balance) "it's unknown"
    Just (asBalance -> realBalance)
      | realBalance == balance -> pass
      | otherwise ->
        Left $
        InvalidBalance (addrToAddrName addr is) (ExpectedBalance balance) $
        "its actual balance is: " <> pretty realBalance
-- | Compose two success validators.
--
-- For example:
--
-- expectBalance bal addr `composeValidators`
-- expectStorageUpdateConst addr2 ValueUnit
composeValidators ::
     SuccessValidator
  -> SuccessValidator
  -> SuccessValidator
composeValidators val1 val2 gState updates =
  val1 gState updates >> val2 gState updates

-- | Compose a list of success validators.
composeValidatorsList :: [SuccessValidator] -> SuccessValidator
composeValidatorsList = foldl' composeValidators expectAnySuccess

-- | Check that interpreter failed due to gas exhaustion.
expectGasExhaustion :: InterpreterError -> Bool
expectGasExhaustion =
  \case
    IEInterpreterFailed _ (RuntimeFailure (MichelsonGasExhaustion, _)) -> True
    _ -> False

-- | Expect that interpretation of contract with given address ended
-- with [FAILED].
expectMichelsonFailed :: (MichelsonFailed -> Bool) -> Address -> InterpreterError -> Bool
expectMichelsonFailed predicate addr =
  \case
    IEInterpreterFailed failedAddr (RuntimeFailure (mf, _)) ->
      addr == failedAddr && predicate mf
    _ -> False

----------------------------------------------------------------------------
-- Implementation of the testing engine
----------------------------------------------------------------------------

initIS :: InternalState
initIS = InternalState
  { _isNow = dummyNow
  , _isMaxSteps = dummyMaxSteps
  , _isGState = initGState
  , _isOperations = mempty
  , _isContractsNames = Map.empty
  , _isSource = Nothing
  }

integrationalTest ::
     (Maybe ValidationError -> res)
  -> IntegrationalScenario
  -> res
integrationalTest howToFail scenario =
  howToFail $ leftToMaybe $ runExcept (runStateT scenario initIS)

validateResult ::
     IntegrationalValidator
  -> Either InterpreterError InterpreterRes
  -> InternalState
  -> Except ValidationError (Maybe GState)
validateResult validator result iState =
  case (validator, result) of
    (Left validateError, Left err)
      | validateError err -> pure Nothing
    (_, Left err) ->
      doFail $ UnexpectedInterpreterError $ mkError err iState
    (Left _, Right _) ->
      doFail $ ExpectingInterpreterToFail
    (Right validateUpdates, Right ir)
      | Left bad <- validateUpdates iState (_irGState ir) (_irUpdates ir) ->
        doFail $ IncorrectUpdates bad (_irUpdates ir)
      | otherwise -> pure $ Just $ _irGState ir
  where
    doFail = throwError
    mkError
      :: InterpreterError -> InternalState -> IntegrationalInterpreterError
    mkError iErr is = case iErr of
      IEUnknownContract addr -> IEUnknownContract $ addrToAddrName addr is
      IEInterpreterFailed addr err ->
        IEInterpreterFailed (addrToAddrName addr is) err
      IEAlreadyOriginated addr cs ->
        IEAlreadyOriginated (addrToAddrName addr is) cs
      IEUnknownSender addr -> IEUnknownSender $ addrToAddrName addr is
      IEUnknownManager addr -> IEUnknownManager $ addrToAddrName addr is
      IENotEnoughFunds addr amount ->
        IENotEnoughFunds (addrToAddrName addr is) amount
      IEFailedToApplyUpdates err -> IEFailedToApplyUpdates err
      IEIllTypedContract err -> IEIllTypedContract err
