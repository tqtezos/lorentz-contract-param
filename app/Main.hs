module Main
  ( main
  ) where


import Data.Version (showVersion)
import Fmt (pretty)
import Named ((!))
import Options.Applicative
  (auto, command, eitherReader, execParser, fullDesc, header, help, helper, info, infoOption, long,
  maybeReader, metavar, option, progDesc, readerError, short, showDefault, showDefaultWith,
  strOption, subparser, switch, value)
import qualified Options.Applicative as Opt
import Paths_morley (version)
import Text.Pretty.Simple (pPrint)

import Michelson.Untyped hiding (OriginationOperation(..))
import qualified Michelson.Untyped as Un
import Morley.Ext (typeCheckMorleyContract)
import Morley.Macro (expandFlattenContract, expandValue)
import qualified Morley.Parser as P
import Morley.Runtime
  (TxData(..), originateContract, prepareContract, readAndParseContract, runContract, transfer)
import Morley.Runtime.GState (genesisAddress, genesisKeyHash)
import Tezos.Address (Address, parseAddress)
import Tezos.Core
  (Mutez, Timestamp(..), mkMutez, parseTimestamp, timestampFromSeconds, unMutez, unsafeMkMutez)
import Tezos.Crypto

data CmdLnArgs
  = Parse (Maybe FilePath) Bool
  | TypeCheck (Maybe FilePath) Bool
  | Run !RunOptions
  | Originate !OriginateOptions
  | Transfer !TransferOptions

data RunOptions = RunOptions
  { roContractFile :: !(Maybe FilePath)
  , roDBPath :: !FilePath
  , roStorageValue :: !(Value Op)
  , roTxData :: !TxData
  , roVerbose :: !Bool
  , roNow :: !(Maybe Timestamp)
  , roMaxSteps :: !Word64
  , roInitBalance :: !Mutez
  , roWrite :: !Bool
  }

data OriginateOptions = OriginateOptions
  { ooContractFile :: !(Maybe FilePath)
  , ooDBPath :: !FilePath
  , ooManager :: !KeyHash
  , ooDelegate :: !(Maybe KeyHash)
  , ooSpendable :: !Bool
  , ooDelegatable :: !Bool
  , ooStorageValue :: !(Value Op)
  , ooBalance :: !Mutez
  , ooVerbose :: !Bool
  }

data TransferOptions = TransferOptions
  { toDBPath :: !FilePath
  , toDestination :: !Address
  , toTxData :: !TxData
  , toNow :: !(Maybe Timestamp)
  , toMaxSteps :: !Word64
  , toVerbose :: !Bool
  , toDryRun :: !Bool
  }

argParser :: Opt.Parser CmdLnArgs
argParser = subparser $
  parseSubCmd <>
  typecheckSubCmd <>
  runSubCmd <>
  originateSubCmd <>
  transferSubCmd
  where
    mkCommandParser commandName parser desc =
      command commandName $
      info (helper <*> parser) $
      progDesc desc

    parseSubCmd =
      mkCommandParser "parse"
      (uncurry Parse <$> parseOptions)
      "Parse passed contract"

    typecheckSubCmd =
      mkCommandParser "typecheck"
      (uncurry TypeCheck <$> typecheckOptions)
      "Typecheck passed contract"

    runSubCmd =
      mkCommandParser "run"
      (Run <$> runOptions) $
      "Run passed contract. \
      \It's originated first and then a transaction is sent to it"

    originateSubCmd =
      mkCommandParser "originate"
      (Originate <$> originateOptions)
      "Originate passed contract. Add it to passed DB"

    transferSubCmd =
      mkCommandParser "transfer"
      (Transfer <$> transferOptions)
      "Transfer tokens to given address"

    verboseFlag :: Opt.Parser Bool
    verboseFlag = switch $
      short 'v' <>
      long "verbose" <>
      help "Whether output should be verbose"

    writeFlag :: Opt.Parser Bool
    writeFlag = switch $
      long "write" <>
      help "Whether updated DB should be written to DB file"

    dryRunFlag :: Opt.Parser Bool
    dryRunFlag = switch $
      long "dry-run" <>
      help "Do not write updated DB to DB file"

    typecheckOptions :: Opt.Parser (Maybe FilePath, Bool)
    typecheckOptions = (,)
      <$> contractFileOption
      <*> verboseFlag

    parseOptions :: Opt.Parser (Maybe FilePath, Bool)
    parseOptions = (,)
      <$> contractFileOption
      <*> switch (
        long "expand-macros" <>
        help "Whether expand macros after parsing or not")

    defaultBalance :: Mutez
    defaultBalance = unsafeMkMutez 4000000

    runOptions :: Opt.Parser RunOptions
    runOptions =
      RunOptions
        <$> contractFileOption
        <*> dbPathOption
        <*> valueOption "storage" "Initial storage of a running contract"
        <*> txData
        <*> verboseFlag
        <*> nowOption
        <*> maxStepsOption
        <*> mutezOption (Just defaultBalance)
            "balance" "Initial balance of this contract"
        <*> writeFlag

    originateOptions :: Opt.Parser OriginateOptions
    originateOptions =
      OriginateOptions
        <$> contractFileOption
        <*> dbPathOption
        <*> keyHashOption (Just genesisKeyHash) "manager" "Contract's manager"
        <*> optional
            (keyHashOption Nothing "manager" "Contract's optional delegate")
        <*> switch (long "spendable" <>
                    help "Whether the contract is spendable")
        <*> switch (long "delegatable" <>
                    help "Whether the contract is delegatable")
        <*> valueOption "storage" "Initial storage of an originating contract"
        <*> mutezOption (Just defaultBalance)
            "balance" "Initial balance of an originating contract"
        <*> verboseFlag

    transferOptions :: Opt.Parser TransferOptions
    transferOptions = do
      toDBPath <- dbPathOption
      toDestination <- addressOption Nothing "to" "Destination address"
      toTxData <- txData
      toNow <- nowOption
      toMaxSteps <- maxStepsOption
      toVerbose <- verboseFlag
      toDryRun <- dryRunFlag
      pure TransferOptions {..}

contractFileOption :: Opt.Parser (Maybe FilePath)
contractFileOption = optional $ strOption $
  long "contract" <>
  metavar "FILEPATH" <>
  help "Path to contract file"

nowOption :: Opt.Parser (Maybe Timestamp)
nowOption = optional $ option parser $
  long "now" <>
  metavar "TIMESTAMP" <>
  help "Timestamp that you want the runtime interpreter to use (default is now)"
  where
    parser =
      (timestampFromSeconds @Integer <$> auto) <|>
      maybeReader (parseTimestamp . toText)

maxStepsOption :: Opt.Parser Word64
maxStepsOption = option auto $
  value 100500 <>
  long "max-steps" <>
  metavar "Word64" <>
  help "Max steps that you want the runtime interpreter to use" <>
  showDefault

dbPathOption :: Opt.Parser FilePath
dbPathOption = strOption $
  long "db" <>
  metavar "FILEPATH" <>
  value "db.json" <>
  help "Path to DB with data which is used instead of real blockchain data"

keyHashOption :: Maybe KeyHash -> String -> String -> Opt.Parser KeyHash
keyHashOption defaultValue name hInfo =
  option (eitherReader (first pretty . parseKeyHash . toText)) $
  long name <>
  maybe mempty value defaultValue <>
  help hInfo

valueOption :: String -> String -> Opt.Parser (Value Op)
valueOption name hInfo = option (eitherReader parseValue) $
  long name <>
  help hInfo
  where
    parseValue :: String -> Either String (Value Op)
    parseValue s =
      either (Left . mappend "Failed to parse value: " . show)
             (Right . expandValue)
      $ P.parseNoEnv P.value "" (toText s)

mutezOption :: Maybe Mutez -> String -> String -> Opt.Parser Mutez
mutezOption defaultValue name hInfo =
  option (maybe (readerError "Invalid mutez") pure . mkMutez =<< auto) $
  long name <>
  metavar "INT" <>
  maybe mempty (mappend (showDefaultWith (show . unMutez)) . value) defaultValue <>
  help hInfo

addressOption :: Maybe Address -> String -> String -> Opt.Parser Address
addressOption defAddress name hInfo =
  option (eitherReader parseAddrDo) $ mconcat
  [ long name
  , metavar "ADDRESS"
  , help hInfo
  , maybe mempty defaults defAddress
  ]
  where
    defaults addr = value addr <> showDefaultWith pretty
    parseAddrDo addr =
      either (Left . mappend "Failed to parse address: " . pretty) Right $
      parseAddress $ toText addr

txData :: Opt.Parser TxData
txData =
  mkTxData
    <$> addressOption (Just genesisAddress) "sender" "Sender address"
    <*> valueOption "parameter" "Parameter of passed contract"
    <*> mutezOption (Just minBound) "amount" "Amout sent by a transaction"
  where
    mkTxData :: Address -> Value Op -> Mutez -> TxData
    mkTxData addr param amount =
      TxData
        { tdSenderAddress = addr
        , tdParameter = param
        , tdAmount = amount
        }

main :: IO ()
main = do
  cmdLnArgs <- execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    programInfo = info (helper <*> versionOption <*> argParser) $
      mconcat
      [ fullDesc
      , progDesc "Morley: Haskell implementation of Michelson typechecker and interpreter"
      , header "Morley tools"
      ]

    versionOption = infoOption ("morley-" <> showVersion version)
      (long "version" <> help "Show version.")

    run :: CmdLnArgs -> IO ()
    run args = case args of
      Parse mFilename hasExpandMacros -> do
        contract <- readAndParseContract mFilename
        if hasExpandMacros
          then pPrint $ expandFlattenContract contract
          else pPrint contract
      TypeCheck mFilename _hasVerboseFlag -> do
        michelsonContract <- prepareContract mFilename
        void $ either throwM pure $
          (typeCheckMorleyContract . fmap unOp) michelsonContract
        putTextLn "Contract is well-typed"
      Run RunOptions {..} -> do
        michelsonContract <- prepareContract roContractFile
        runContract roNow roMaxSteps roInitBalance roDBPath roStorageValue michelsonContract roTxData
          ! #verbose roVerbose
          ! #dryRun (not roWrite)
      Originate OriginateOptions {..} -> do
        michelsonContract <- prepareContract ooContractFile
        let origination = Un.OriginationOperation
              { Un.ooManager = ooManager
              , Un.ooDelegate = ooDelegate
              , Un.ooSpendable = ooSpendable
              , Un.ooDelegatable = ooDelegatable
              , Un.ooStorage = ooStorageValue
              , Un.ooBalance = ooBalance
              , Un.ooContract = michelsonContract
              }
        addr <- originateContract ooDBPath origination ! #verbose ooVerbose
        putTextLn $ "Originated contract " <> pretty addr
      Transfer TransferOptions {..} -> do
        transfer toNow toMaxSteps toDBPath toDestination toTxData
          ! #verbose toVerbose
          ! #dryRun toDryRun
