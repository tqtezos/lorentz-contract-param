module Main
  ( main
  ) where


import Data.Version (showVersion)
import Fmt (pretty)
import Named ((!))
import Options.Applicative
  (auto, command, eitherReader, execParser, footerDoc, fullDesc, header, help, helper, info,
  infoOption, long, maybeReader, metavar, option, progDesc, readerError, short, showDefault,
  showDefaultWith, strOption, subparser, switch, value)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_morley (version)
import Text.Pretty.Simple (pPrint)

import Michelson.Macro (expandContract, expandValue)
import qualified Michelson.Parser as P
import Michelson.Printer (printUntypedContract)
import Michelson.Runtime
  (TxData(..), originateContract, prepareContract, readAndParseContract, runContract, transfer,
  typeCheckWithDb)
import Michelson.Runtime.GState (genesisAddress, genesisKeyHash)
import Michelson.Untyped hiding (OriginationOperation(..))
import qualified Michelson.Untyped as U
import Tezos.Address (Address, parseAddress)
import Tezos.Core
  (Mutez, Timestamp(..), mkMutez, parseTimestamp, timestampFromSeconds, unMutez, unsafeMkMutez)
import Tezos.Crypto

data CmdLnArgs
  = Parse (Maybe FilePath) Bool
  | Print (Maybe FilePath)
  | TypeCheck !TypeCheckOptions
  | Run !RunOptions
  | Originate !OriginateOptions
  | Transfer !TransferOptions

data TypeCheckOptions = TypeCheckOptions
  { tcoContractFile :: !(Maybe FilePath)
  , tcoDBPath       :: !FilePath
  , tcoVerbose      :: !Bool
  }

data RunOptions = RunOptions
  { roContractFile :: !(Maybe FilePath)
  , roDBPath :: !FilePath
  , roStorageValue :: !U.Value
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
  , ooStorageValue :: !U.Value
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
  printSubCmd <>
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
        (TypeCheck <$> typeCheckOptions) $
        ("Typecheck passed contract")

    printSubCmd =
      mkCommandParser "print"
      (Print <$> printOptions)
      ("Parse a Morley contract and print corresponding Michelson " <>
       "contract that can be parsed the OCaml reference client")

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

    typeCheckOptions :: Opt.Parser TypeCheckOptions
    typeCheckOptions = TypeCheckOptions
      <$> contractFileOption
      <*> dbPathOption
      <*> verboseFlag

    parseOptions :: Opt.Parser (Maybe FilePath, Bool)
    parseOptions = (,)
      <$> contractFileOption
      <*> switch (
        long "expand-macros" <>
        help "Whether expand macros after parsing or not")

    defaultBalance :: Mutez
    defaultBalance = unsafeMkMutez 4000000

    printOptions :: Opt.Parser (Maybe FilePath)
    printOptions = contractFileOption

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
            (keyHashOption Nothing "delegate" "Contract's optional delegate")
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
  metavar "INT" <>
  help "Max steps that you want the runtime interpreter to use" <>
  showDefault

dbPathOption :: Opt.Parser FilePath
dbPathOption = strOption $
  long "db" <>
  metavar "FILEPATH" <>
  value "db.json" <>
  help "Path to DB with data which is used instead of real blockchain data" <>
  showDefault

keyHashOption :: Maybe KeyHash -> String -> String -> Opt.Parser KeyHash
keyHashOption defaultValue name hInfo =
  option (eitherReader (first pretty . parseKeyHash . toText)) $
  long name <>
  maybeAddDefault pretty defaultValue <>
  help hInfo

valueOption :: String -> String -> Opt.Parser U.Value
valueOption name hInfo = option (eitherReader parseValue) $
  long name <>
  help hInfo
  where
    parseValue :: String -> Either String U.Value
    parseValue s =
      either (Left . mappend "Failed to parse value: " . show)
             (Right . expandValue)
      $ P.parseNoEnv P.value "" (toText s)

mutezOption :: Maybe Mutez -> String -> String -> Opt.Parser Mutez
mutezOption defaultValue name hInfo =
  option (maybe (readerError "Invalid mutez") pure . mkMutez =<< auto) $
  long name <>
  metavar "INT" <>
  maybeAddDefault (show . unMutez) defaultValue <>
  help hInfo

addressOption :: Maybe Address -> String -> String -> Opt.Parser Address
addressOption defAddress name hInfo =
  option (eitherReader parseAddrDo) $ mconcat
  [ long name
  , metavar "ADDRESS"
  , help hInfo
  , maybeAddDefault pretty defAddress
  ]
  where
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
    mkTxData :: Address -> Value -> Mutez -> TxData
    mkTxData addr param amount =
      TxData
        { tdSenderAddress = addr
        , tdParameter = param
        , tdAmount = amount
        }

-- Maybe add default value and make sure it will be shown in help message.
maybeAddDefault :: Opt.HasValue f => (a -> String) -> Maybe a -> Opt.Mod f a
maybeAddDefault printer = maybe mempty addDefault
  where
    addDefault v = value v <> showDefaultWith printer

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
      , footerDoc $ usageDoc
      ]

    versionOption = infoOption ("morley-" <> showVersion version)
      (long "version" <> help "Show version.")

    run :: CmdLnArgs -> IO ()
    run args = case args of
      Parse mFilename hasExpandMacros -> do
        contract <- readAndParseContract mFilename
        if hasExpandMacros
          then pPrint $ expandContract contract
          else pPrint contract
      Print mFilename -> do
        contract <- prepareContract mFilename
        putStrLn $ printUntypedContract contract
      TypeCheck TypeCheckOptions{..} -> do
        morleyContract <- prepareContract tcoContractFile
        either throwM (const pass) =<< typeCheckWithDb tcoDBPath morleyContract
        putTextLn "Contract is well-typed"
      Run RunOptions {..} -> do
        michelsonContract <- prepareContract roContractFile
        runContract roNow roMaxSteps roInitBalance roDBPath roStorageValue michelsonContract roTxData
          ! #verbose roVerbose
          ! #dryRun (not roWrite)
      Originate OriginateOptions {..} -> do
        michelsonContract <- prepareContract ooContractFile
        let origination = U.OriginationOperation
              { U.ooManager = ooManager
              , U.ooDelegate = ooDelegate
              , U.ooSpendable = ooSpendable
              , U.ooDelegatable = ooDelegatable
              , U.ooStorage = ooStorageValue
              , U.ooBalance = ooBalance
              , U.ooContract = michelsonContract
              }
        addr <- originateContract ooDBPath origination ! #verbose ooVerbose
        putTextLn $ "Originated contract " <> pretty addr
      Transfer TransferOptions {..} -> do
        transfer toNow toMaxSteps toDBPath toDestination toTxData
          ! #verbose toVerbose
          ! #dryRun toDryRun

usageDoc :: Maybe Doc
usageDoc = Just $ mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  morley run --help", linebreak
   , linebreak
   , "Documentation for morley tools can be found at the following links:", linebreak
   , "  https://gitlab.com/morley-framework/morley/blob/master/README.md", linebreak
   , "  https://gitlab.com/morley-framework/morley/tree/master/docs", linebreak
   , linebreak
   , "Sample contracts for running can be found at the following link:", linebreak
   , "  https://gitlab.com/morley-framework/morley/tree/master/contracts", linebreak
   , linebreak
   , "USAGE EXAMPLE:", linebreak
   , "  morley parse --contract add1.tz", linebreak
   , linebreak
   , "  This command will parse contract stored in add1.tz", linebreak
   , "  and return its representation in haskell types", linebreak
   , linebreak
   , "  morley originate --contract add1.tz --storage 1 --verbose", linebreak
   , linebreak
   , "  This command will originate contract with code stored in add1.tz", linebreak
   , "  with initial storage value set to 1 and return info about", linebreak
   , "  originated contract: its balance, storage and contract code"]
