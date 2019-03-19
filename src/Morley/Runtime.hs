-- | Interpreter of a contract in Morley language.

module Morley.Runtime
       ( originateContract
       , runContract

       -- * Re-exports
       , ContractState (..)
       , AddressState (..)
       , TxData (..)

       -- * For testing
       , InterpreterOp (..)
       , InterpreterRes (..)
       , InterpreterError (..)
       , interpreterPure
       ) where

import Control.Lens (at, makeLenses, (%=), (.=), (<>=))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Fmt (Buildable(build), blockListF, fmtLn, nameF, pretty, (+|), (|+))

import Michelson.Interpret
  (ContractEnv(..), InterpretUntypedError(..), InterpretUntypedResult(..), InterpreterState(..),
  RemainingSteps(..))
import Michelson.Typed
  (CreateContract(..), Instr, Operation(..), TransferTokens(..), Val(..), convertContract,
  unsafeValToValue)
import Michelson.Untyped (Contract(..), Op, OriginationOperation(..), Value, mkContractAddress)
import Morley.Aliases (UntypedContract)
import Morley.Ext (interpretMorleyUntyped)
import Morley.Runtime.GState
import Morley.Runtime.TxData
import Morley.Types (MorleyLogs(..), noMorleyLogs)
import Tezos.Address (Address(..))
import Tezos.Core
  (Mutez, Timestamp(..), getCurrentTime, unsafeAddMutez, unsafeMkMutez, unsafeSubMutez)

----------------------------------------------------------------------------
-- Auxiliary types
----------------------------------------------------------------------------

-- | Operations executed by interpreter.
-- In our model one Michelson's operation (`operation` type in Michelson)
-- corresponds to 0 or 1 interpreter operation.
--
-- Note: 'Address' is not part of 'TxData', because 'TxData' is
-- supposed to be provided by the user, while 'Address' can be
-- computed by our code.
data InterpreterOp
  = OriginateOp !OriginationOperation
  -- ^ Originate a contract.
  | TransferOp Address
               TxData
  -- ^ Send a transaction to given address which is assumed to be the
  -- address of an originated contract.
  deriving (Show)

-- | Result of a single execution of interpreter.
data InterpreterRes = InterpreterRes
  { _irGState :: !GState
  -- ^ New 'GState'.
  , _irOperations :: [InterpreterOp]
  -- ^ List of operations to be added to the operations queue.
  , _irUpdates :: ![GStateUpdate]
  -- ^ Updates applied to 'GState'.
  , _irPrintedLogs :: [MorleyLogs]
  -- ^ During execution a contract can print logs,
  -- all logs are kept until all called contracts are executed
  , _irSourceAddress :: !(Maybe Address)
  -- ^ As soon as transfer operation is encountered, this address is
  -- set to its input.
  , _irRemainingSteps :: !RemainingSteps
  } deriving (Show)

makeLenses ''InterpreterRes

-- | Errors that can happen during contract interpreting.
data InterpreterError
  = IEUnknownContract !Address
  -- ^ The interpreted contract hasn't been originated.
  | IEInterpreterFailed !(Contract Op)
                        !(InterpretUntypedError MorleyLogs)
  -- ^ Interpretation of Michelson contract failed.
  | IEAlreadyOriginated !Address
                        !ContractState
  -- ^ A contract is already originated.
  | IEUnknownSender !Address
  -- ^ Sender address is unknown.
  | IEUnknownManager !Address
  -- ^ Manager address is unknown.
  | IENotEnoughFunds !Address !Mutez
  -- ^ Sender doesn't have enough funds.
  | IEFailedToApplyUpdates !GStateUpdateError
  -- ^ Failed to apply updates to GState.
  deriving (Show)

instance Buildable InterpreterError where
  build =
    \case
      IEUnknownContract addr -> "The contract is not originated " +| addr |+ ""
      IEInterpreterFailed _ err -> "Michelson interpreter failed: " +| err |+ ""
      IEAlreadyOriginated addr cs ->
        "The following contract is already originated: " +| addr |+
        ", " +| cs |+ ""
      IEUnknownSender addr -> "The sender address is unknown " +| addr |+ ""
      IEUnknownManager addr -> "The manager address is unknown " +| addr |+ ""
      IENotEnoughFunds addr amount ->
        "The sender (" +| addr |+
        ") doesn't  have enough funds (has only " +| amount |+ ")"
      IEFailedToApplyUpdates err -> "Failed to update GState: " +| err |+ ""

instance Exception InterpreterError where
  displayException = pretty

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- | Originate a contract. Returns the address of the originated
-- contract.
originateContract :: Bool -> FilePath -> OriginationOperation -> IO Address
originateContract verbose dbPath origination =
  mkContractAddress origination <$
  interpreter Nothing 100500 verbose dbPath (OriginateOp origination)

-- | Run a contract. The contract is originated first (if it's not
-- already) and then we pretend that we send a transaction to it.
runContract
    :: Maybe Timestamp
    -> Word64
    -> Bool
    -> FilePath
    -> Value Op
    -> Contract Op
    -> TxData
    -> IO ()
runContract maybeNow maxSteps verbose dbPath storageValue contract txData = do
  addr <- originateContract verbose dbPath origination
    `catch` ignoreAlreadyOriginated
  interpreter maybeNow maxSteps verbose dbPath (TransferOp addr txData)
  where
    defaultBalance = unsafeMkMutez 4000000000
    origination = OriginationOperation
      { ooManager = genesisKeyHash
      , ooDelegate = Nothing
      , ooSpendable = False
      , ooDelegatable = False
      , ooBalance = defaultBalance
      , ooStorage = storageValue
      , ooContract = contract
      }
    ignoreAlreadyOriginated :: InterpreterError -> IO Address
    ignoreAlreadyOriginated =
      \case IEAlreadyOriginated addr _ -> pure addr
            err -> throwM err

----------------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------------

-- | Interpret a contract on some global state (read from file) and
-- transaction data (passed explicitly).
interpreter :: Maybe Timestamp -> Word64 -> Bool -> FilePath -> InterpreterOp -> IO ()
interpreter maybeNow maxSteps verbose dbPath operation = do
  now <- maybe getCurrentTime pure maybeNow
  gState <- readGState dbPath
  let eitherRes =
        interpreterPure now (RemainingSteps maxSteps) gState [operation]
  InterpreterRes {..} <- either throwM pure eitherRes
  when (verbose && not (null _irUpdates)) $ do
    fmtLn $ nameF "Updates:" (blockListF _irUpdates)
    putTextLn $ "Remaining gas: " <> pretty _irRemainingSteps
  forM_ _irPrintedLogs $ \(MorleyLogs logs) -> do
    mapM_ putTextLn logs
    putTextLn "" -- extra break line to separate logs from two sequence contracts
  writeGState dbPath _irGState

-- | Implementation of interpreter outside 'IO'.  It reads operations,
-- interprets them one by one and updates state accordingly.
interpreterPure ::
  Timestamp -> RemainingSteps -> GState -> [InterpreterOp] -> Either InterpreterError InterpreterRes
interpreterPure now maxSteps gState ops =
    runExcept (execStateT (statefulInterpreter now) initialState)
  where
    initialState = InterpreterRes
      { _irGState = gState
      , _irOperations = ops
      , _irUpdates = mempty
      , _irPrintedLogs = def
      , _irSourceAddress = Nothing
      , _irRemainingSteps = maxSteps
      }

statefulInterpreter
  :: Timestamp
  -> StateT InterpreterRes (Except InterpreterError) ()
statefulInterpreter now = do
  curGState <- use irGState
  mSourceAddr <- use irSourceAddress
  remainingSteps <- use irRemainingSteps
  use irOperations >>= \case
    [] -> pass
    (op:opsTail) ->
      either throwError (processIntRes opsTail) $
      interpretOneOp now remainingSteps mSourceAddr curGState op
  where
    processIntRes opsTail InterpreterRes {..} = do
      irGState .= _irGState
      irOperations .= opsTail <> _irOperations
      irUpdates <>= _irUpdates
      irPrintedLogs <>= _irPrintedLogs
      irSourceAddress %= (<|> _irSourceAddress)
      irRemainingSteps .= _irRemainingSteps
      statefulInterpreter now

-- | Run only one interpreter operation and update 'GState' accordingly.
interpretOneOp
  :: Timestamp
  -> RemainingSteps
  -> Maybe Address
  -> GState
  -> InterpreterOp
  -> Either InterpreterError InterpreterRes
interpretOneOp _ remainingSteps _ gs (OriginateOp origination) = do
  let originatorAddress = KeyAddress (ooManager origination)
  originatorBalance <- case gsAddresses gs ^. at (originatorAddress) of
    Nothing -> Left (IEUnknownManager originatorAddress)
    Just (asBalance -> oldBalance)
      | oldBalance < ooBalance origination ->
        Left (IENotEnoughFunds originatorAddress oldBalance)
      | otherwise ->
        -- Subtraction is safe because we have checked its
        -- precondition in guard.
        Right (oldBalance `unsafeSubMutez` ooBalance origination)
  let
    updates =
      [ GSAddAddress address (ASContract contractState)
      , GSSetBalance originatorAddress originatorBalance
      ]
  case applyUpdates updates gs of
    Left _ -> Left (IEAlreadyOriginated address contractState)
    Right newGS -> Right $
      InterpreterRes
      { _irGState = newGS
      , _irOperations = mempty
      , _irUpdates = updates
      , _irPrintedLogs = def
      , _irSourceAddress = Nothing
      , _irRemainingSteps = remainingSteps
      }
  where
    contractState = ContractState
      { csBalance = ooBalance origination
      , csStorage = ooStorage origination
      , csContract = ooContract origination
      }
    address = mkContractAddress origination
interpretOneOp now remainingSteps mSourceAddr gs (TransferOp addr txData) = do
    let sourceAddr = fromMaybe (tdSenderAddress txData) mSourceAddr
    let senderAddr = tdSenderAddress txData
    decreaseSenderBalance <- case addresses ^. at senderAddr of
      Nothing -> Left (IEUnknownSender senderAddr)
      Just (asBalance -> balance)
        | balance < tdAmount txData ->
          Left (IENotEnoughFunds senderAddr balance)
        | otherwise ->
          -- Subtraction is safe because we have checked its
          -- precondition in guard.
          Right (GSSetBalance senderAddr (balance `unsafeSubMutez` tdAmount txData))
    let onlyUpdates updates = Right (updates, [], noMorleyLogs, remainingSteps)
    (otherUpdates, sideEffects, logs, newRemSteps) <- case (addresses ^. at addr, addr) of
      (Nothing, ContractAddress _) ->
        Left (IEUnknownContract addr)
      (Nothing, KeyAddress _) -> do
        let
          addrState = ASSimple (tdAmount txData)
          upd = GSAddAddress addr addrState
        onlyUpdates [upd]
      (Just (ASSimple oldBalance), _) -> do
        -- can't overflow if global state is correct (because we can't
        -- create money out of nowhere)
        let
          newBalance = oldBalance `unsafeAddMutez` tdAmount txData
          upd = GSSetBalance addr newBalance
        onlyUpdates [upd]
      (Just (ASContract cs), _) -> do
        let
          contract = csContract cs
          contractEnv = ContractEnv
            { ceNow = now
            , ceMaxSteps = remainingSteps
            , ceBalance = csBalance cs
            , ceContracts = Map.mapMaybe extractContract addresses
            , ceSelf = addr
            , ceSource = sourceAddr
            , ceSender = senderAddr
            , ceAmount = tdAmount txData
            }
        InterpretUntypedResult
          { iurOps = sideEffects
          , iurNewStorage = newValue
          , iurNewState = InterpreterState printedLogs newRemainingSteps
          }
          <- first (IEInterpreterFailed contract) $
                interpretMorleyUntyped contract (tdParameter txData)
                                 (csStorage cs) contractEnv
        let
          newValueU = unsafeValToValue newValue
          -- can't overflow if global state is correct (because we can't
          -- create money out of nowhere)
          newBalance = csBalance cs `unsafeAddMutez` tdAmount txData
          updBalance = GSSetBalance addr newBalance
          updStorage = GSSetStorageValue addr newValueU
          updates =
            [ updBalance
            , updStorage
            ]
        Right (updates, sideEffects, printedLogs, newRemainingSteps)

    let
      updates = decreaseSenderBalance:otherUpdates

    newGState <- first IEFailedToApplyUpdates $ applyUpdates updates gs

    return InterpreterRes
      { _irGState = newGState
      , _irOperations = mapMaybe (convertOp addr) sideEffects
      , _irUpdates = updates
      , _irPrintedLogs = [logs]
      , _irSourceAddress = Just sourceAddr
      , _irRemainingSteps = newRemSteps
      }
  where
    addresses :: Map Address AddressState
    addresses = gsAddresses gs

    extractContract :: AddressState -> Maybe UntypedContract
    extractContract =
      \case ASSimple {} -> Nothing
            ASContract cs -> Just (csContract cs)

----------------------------------------------------------------------------
-- Simple helpers
----------------------------------------------------------------------------

-- The argument is the address of the contract that generation this operation.
convertOp :: Address -> Operation Instr -> Maybe InterpreterOp
convertOp interpretedAddr =
  \case
    OpTransferTokens tt ->
      let txData =
            TxData
              { tdSenderAddress = interpretedAddr
              , tdParameter = unsafeValToValue (ttContractParameter tt)
              , tdAmount = ttAmount tt
              }
          VContract destAddress = ttContract tt
       in Just (TransferOp destAddress txData)
    OpSetDelegate {} -> Nothing
    OpCreateAccount {} -> Nothing
    OpCreateContract cc ->
      let origination = OriginationOperation
            { ooManager = ccManager cc
            , ooDelegate = ccDelegate cc
            , ooSpendable = ccSpendable cc
            , ooDelegatable = ccDelegatable cc
            , ooBalance = ccBalance cc
            , ooStorage = unsafeValToValue (ccStorageVal cc)
            , ooContract = convertContract (ccContractCode cc)
            }
       in Just (OriginateOp origination)
