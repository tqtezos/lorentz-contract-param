-- | Interpreter and typechecker of a contract in Morley language.

module Morley.Runtime
       (
         -- * High level interface for end user
         originateContract
       , runContract
       , transfer

       -- * Other helpers
       , parseContract
       , parseExpandContract
       , readAndParseContract
       , prepareContract
       , typeCheckWithDb

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
import Data.Text.IO (getContents)
import Fmt (Buildable(build), blockListF, fmt, fmtLn, nameF, pretty, (+|), (|+))
import Named ((:!), (:?), arg, argDef, defaults, (!))
import Text.Megaparsec (parse)

import Michelson.Interpret
  (ContractEnv(..), InterpretUntypedError(..), InterpretUntypedResult(..), InterpreterState(..),
  RemainingSteps(..))
import Michelson.TypeCheck (SomeContract, TCError)
import Michelson.Typed
  (CreateContract(..), Instr, Operation(..), TransferTokens(..), convertContract, untypeValue)
import qualified Michelson.Typed as T
import Michelson.Untyped (Contract, OriginationOperation(..), mkContractAddress)
import qualified Michelson.Untyped as U
import Morley.Ext (interpretMorleyUntyped, typeCheckMorleyContract)
import Morley.Macro (expandContract)
import qualified Morley.Parser as P
import Morley.Runtime.GState
import Morley.Runtime.TxData
import Morley.Types (MorleyLogs(..), ParsedOp)
import Tezos.Address (Address(..))
import Tezos.Core (Mutez, Timestamp(..), getCurrentTime, unsafeAddMutez, unsafeSubMutez)
import Tezos.Crypto (parseKeyHash)

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
  , _irInterpretResults :: [(Address, InterpretUntypedResult MorleyLogs)]
  -- ^ During execution a contract can print logs and in the end it returns
  -- a pair. All logs and returned values are kept until all called contracts
  -- are executed. In the end they are printed.
  , _irSourceAddress :: !(Maybe Address)
  -- ^ As soon as transfer operation is encountered, this address is
  -- set to its input.
  , _irRemainingSteps :: !RemainingSteps
  -- ^ Now much gas all remaining executions can consume.
  } deriving (Show)

makeLenses ''InterpreterRes

-- | Errors that can happen during contract interpreting.
data InterpreterError
  = IEUnknownContract !Address
  -- ^ The interpreted contract hasn't been originated.
  | IEInterpreterFailed !Address
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
  | IEIllTypedContract !TCError
  -- ^ A contract is ill-typed.
  deriving (Show)

instance Buildable InterpreterError where
  build =
    \case
      IEUnknownContract addr -> "The contract is not originated " +| addr |+ ""
      IEInterpreterFailed addr err ->
        "Michelson interpreter failed for contract " +| addr |+ ": " +| err |+ ""
      IEAlreadyOriginated addr cs ->
        "The following contract is already originated: " +| addr |+
        ", " +| cs |+ ""
      IEUnknownSender addr -> "The sender address is unknown " +| addr |+ ""
      IEUnknownManager addr -> "The manager address is unknown " +| addr |+ ""
      IENotEnoughFunds addr amount ->
        "The sender (" +| addr |+
        ") doesn't  have enough funds (has only " +| amount |+ ")"
      IEFailedToApplyUpdates err -> "Failed to update GState: " +| err |+ ""
      IEIllTypedContract err -> "The contract is ill-typed " +| err |+ ""

instance Exception InterpreterError where
  displayException = pretty

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- | Parse a contract from 'Text'.
parseContract ::
     Maybe FilePath -> Text -> Either P.ParserException (U.Contract' ParsedOp)
parseContract mFileName =
  first P.ParserException . parse P.program (fromMaybe "<stdin>" mFileName)

-- | Parse a contract from 'Text' and expand macros.
parseExpandContract ::
     Maybe FilePath -> Text -> Either P.ParserException Contract
parseExpandContract mFileName = fmap expandContract . parseContract mFileName

-- | Read and parse a contract from give path or `stdin` (if the
-- argument is 'Nothing'). The contract is not expanded.
readAndParseContract :: Maybe FilePath -> IO (U.Contract' ParsedOp)
readAndParseContract mFilename = do
  code <- readCode mFilename
  either throwM pure $ parseContract mFilename code
  where
    readCode :: Maybe FilePath -> IO Text
    readCode = maybe getContents readFile

-- | Read a contract using 'readAndParseContract', expand and
-- flatten. The contract is not type checked.
prepareContract :: Maybe FilePath -> IO Contract
prepareContract mFile = expandContract <$> readAndParseContract mFile

-- | Originate a contract. Returns the address of the originated
-- contract.
originateContract ::
     FilePath -> OriginationOperation -> "verbose" :! Bool -> IO Address
originateContract dbPath origination verbose =
  -- pass 100500 as maxSteps, because it doesn't matter for origination,
  -- as well as 'now'
  mkContractAddress origination <$
  interpreter Nothing 100500 dbPath [OriginateOp origination] verbose
  ! defaults

-- | Run a contract. The contract is originated first (if it's not
-- already) and then we pretend that we send a transaction to it.
runContract
  :: Maybe Timestamp
  -> Word64
  -> Mutez
  -> FilePath
  -> U.Value
  -> Contract
  -> TxData
  -> "verbose" :! Bool
  -> "dryRun" :! Bool
  -> IO ()
runContract maybeNow maxSteps initBalance dbPath storageValue contract txData
  verbose (arg #dryRun -> dryRun) =
  interpreter maybeNow maxSteps dbPath operations verbose ! #dryRun dryRun
  where
    -- We hardcode some random key hash here as delegate to make sure that:
    -- 1. Contract's address won't clash with already originated one (because
    -- it may have different storage value which may be confusing).
    -- 2. If one uses this functionality twice with the same contract and
    -- other data, the contract will have the same address.
    delegate =
      either (error . mappend "runContract can't parse delegate: " . pretty) id $
      parseKeyHash "tz1YCABRTa6H8PLKx2EtDWeCGPaKxUhNgv47"
    origination = OriginationOperation
      { ooManager = genesisKeyHash
      , ooDelegate = Just delegate
      , ooSpendable = False
      , ooDelegatable = False
      , ooBalance = initBalance
      , ooStorage = storageValue
      , ooContract = contract
      }
    addr = mkContractAddress origination
    operations =
      [ OriginateOp origination
      , TransferOp addr txData
      ]

-- | Send a transaction to given address with given parameters.
transfer ::
     Maybe Timestamp
  -> Word64
  -> FilePath
  -> Address
  -> TxData
  -> "verbose" :! Bool -> "dryRun" :? Bool -> IO ()
transfer maybeNow maxSteps dbPath destination txData =
  interpreter maybeNow maxSteps dbPath [TransferOp destination txData]

----------------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------------

-- | Interpret a contract on some global state (read from file) and
-- transaction data (passed explicitly).
interpreter ::
     Maybe Timestamp
  -> Word64
  -> FilePath
  -> [InterpreterOp]
  -> "verbose" :! Bool -> "dryRun" :? Bool -> IO ()
interpreter maybeNow maxSteps dbPath operations
  (arg #verbose -> verbose)
  (argDef #dryRun False -> dryRun)
    = do
  now <- maybe getCurrentTime pure maybeNow
  gState <- readGState dbPath
  let eitherRes =
        interpreterPure now (RemainingSteps maxSteps) gState operations
  InterpreterRes {..} <- either throwM pure eitherRes
  mapM_ printInterpretResult _irInterpretResults
  when (verbose && not (null _irUpdates)) $ do
    fmtLn $ nameF "Updates:" (blockListF _irUpdates)
    putTextLn $ "Remaining gas: " <> pretty _irRemainingSteps
  unless dryRun $
    writeGState dbPath _irGState
  where
    printInterpretResult
      :: (Address, InterpretUntypedResult MorleyLogs) -> IO ()
    printInterpretResult (addr, InterpretUntypedResult {..}) = do
      putTextLn $ "Executed contract " <> pretty addr
      case iurOps of
        [] -> putTextLn "It didn't return any operations"
        _ -> fmt $ nameF "It returned operations:" (blockListF iurOps)
      putTextLn $
        "It returned storage: " <> pretty (untypeValue iurNewStorage)
      let MorleyLogs logs = isExtState iurNewState
      unless (null logs) $
        mapM_ putTextLn logs
      putTextLn "" -- extra break line to separate logs from two sequence contracts

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
      , _irInterpretResults = []
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
      irInterpretResults <>= _irInterpretResults
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
  void $ first IEIllTypedContract $
    typeCheckMorleyContract (extractAllContracts gs) (ooContract origination)
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
      , _irInterpretResults = []
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
    let onlyUpdates updates = Right (updates, [], Nothing, remainingSteps)
    (otherUpdates, sideEffects, maybeInterpretRes, newRemSteps)
        <- case (addresses ^. at addr, addr) of
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
          existingContracts = extractAllContracts gs
          contractEnv = ContractEnv
            { ceNow = now
            , ceMaxSteps = remainingSteps
            , ceBalance = csBalance cs
            , ceContracts = existingContracts
            , ceSelf = addr
            , ceSource = sourceAddr
            , ceSender = senderAddr
            , ceAmount = tdAmount txData
            }
        iur@InterpretUntypedResult
          { iurOps = sideEffects
          , iurNewStorage = newValue
          , iurNewState = InterpreterState _ newRemainingSteps
          }
          <- first (IEInterpreterFailed addr) $
                interpretMorleyUntyped contract (tdParameter txData)
                                 (csStorage cs) contractEnv
        let
          newValueU = untypeValue newValue
          -- can't overflow if global state is correct (because we can't
          -- create money out of nowhere)
          newBalance = csBalance cs `unsafeAddMutez` tdAmount txData
          updBalance = GSSetBalance addr newBalance
          updStorage = GSSetStorageValue addr newValueU
          updates =
            [ updBalance
            , updStorage
            ]
        Right (updates, sideEffects, Just iur, newRemainingSteps)

    let
      updates = decreaseSenderBalance:otherUpdates

    newGState <- first IEFailedToApplyUpdates $ applyUpdates updates gs

    return InterpreterRes
      { _irGState = newGState
      , _irOperations = mapMaybe (convertOp addr) sideEffects
      , _irUpdates = updates
      , _irInterpretResults = maybe mempty (one . (addr,)) maybeInterpretRes
      , _irSourceAddress = Just sourceAddr
      , _irRemainingSteps = newRemSteps
      }
  where
    addresses :: Map Address AddressState
    addresses = gsAddresses gs

----------------------------------------------------------------------------
-- TypeCheck
----------------------------------------------------------------------------
typeCheckWithDb
  :: FilePath
  -> U.Contract
  -> IO (Either TCError SomeContract)
typeCheckWithDb dbPath morleyContract = do
  gState <- readGState dbPath
  pure . typeCheckMorleyContract (extractAllContracts gState) $ morleyContract

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
              , tdParameter = untypeValue (ttContractParameter tt)
              , tdAmount = ttAmount tt
              }
          T.VContract destAddress = ttContract tt
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
            , ooStorage = untypeValue (ccStorageVal cc)
            , ooContract = convertContract (ccContractCode cc)
            }
       in Just (OriginateOp origination)

