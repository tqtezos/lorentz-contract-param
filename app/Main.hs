module Main
  ( main
  ) where


import Data.Text.IO (getContents)
import Fmt (pretty)
import Options.Applicative
  (auto, command, eitherReader, execParser, help, info, long, maybeReader, metavar, option,
  progDesc, readerError, showDefault, strOption, subparser, switch, value)
import qualified Options.Applicative as Opt
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)

import Michelson.Untyped hiding (OriginationOperation(..))
import qualified Michelson.Untyped as Un
import Morley.Ext (typeCheckMorleyContract)
import Morley.Macro (expandFlattenContract, expandValue)
import qualified Morley.Parser as P
import Morley.Runtime (TxData(..), originateContract, runContract)
import Morley.Runtime.GState (genesisAddress, genesisKeyHash)
import Morley.Types
import Tezos.Address (Address, parseAddress)
import Tezos.Core (Mutez, Timestamp(..), mkMutez, parseTimestamp, timestampFromSeconds)
import Tezos.Crypto

data CmdLnArgs
  = Parse (Maybe FilePath) Bool
  | TypeCheck (Maybe FilePath) Bool
  | Run !RunOptions
  | Originate !OriginateOptions

data RunOptions = RunOptions
  { roContractFile :: !(Maybe FilePath)
  , roDBPath :: !FilePath
  , roStorageValue :: !(Value Op)
  , roTxData :: !TxData
  , roVerbose :: !Bool
  , roNow :: !(Maybe Timestamp)
  , roMaxSteps :: !Word64
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

argParser :: Opt.Parser CmdLnArgs
argParser = subparser $
  parseSubCmd <>
  typecheckSubCmd <>
  runSubCmd <>
  originateSubCmd
  where
    parseSubCmd = command "parse" $
      info (uncurry Parse <$> parseOptions) $
        progDesc "Parse passed contract"

    typecheckSubCmd = command "typecheck" $
      info (uncurry TypeCheck <$> typecheckOptions) $
        progDesc "Typecheck passed contract"

    runSubCmd = command "run" $
      info (Run <$> runOptions) $
        progDesc "Run passed contract on "

    originateSubCmd = command "originate" $
      info (Originate <$> originateOptions) $
        progDesc "Originate passed contract. Add it to passed DB"

    verboseFlag :: Opt.Parser Bool
    verboseFlag = switch $
      long "verbose" <>
      help "Whether output should be verbose"

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
        <*> mutezOption "balance" "Initial balance of an originating contract"
        <*> verboseFlag

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

mutezOption :: String -> String -> Opt.Parser Mutez
mutezOption name hInfo =
  option (maybe (readerError "Invalid mutez") pure . mkMutez =<< auto) $
  long name <>
  metavar "INT" <>
  help hInfo

txData :: Opt.Parser TxData
txData =
  mkTxData
    <$> sender
    <*> valueOption "parameter" "Parameter of passed contract"
    <*> mutezOption "amount" "Amout sent by a transaction"
  where
    sender = option (eitherReader parseAddrDo) $
      long "sender" <>
      metavar "ADDRESS" <>
      value genesisAddress <>
      help "Sender address"
    parseAddrDo addr =
      either (Left . mappend "Failed to parse address: " . pretty) Right $
        parseAddress $ toText addr
    mkTxData :: Address -> Value Op -> Mutez -> TxData
    mkTxData addr param amount =
      TxData
        { tdSenderAddress = addr
        , tdParameter = param
        , tdAmount = amount
        }

main :: IO ()
main = do
  cmdLnArgs <- execParser (info argParser progInfo)
  run cmdLnArgs `catchAny` (die . displayException)
  where
    progInfo = progDesc "Haskell implementation of Michelson typechecker and interpreter"
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
        runContract roNow roMaxSteps roVerbose roDBPath roStorageValue michelsonContract roTxData
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
        addr <- originateContract ooVerbose ooDBPath origination
        putTextLn $ "Originated contract " <> pretty addr

    readCode :: Maybe FilePath -> IO Text
    readCode = maybe getContents readFile

    readAndParseContract :: Maybe FilePath -> IO (Contract ParsedOp)
    readAndParseContract mFilename = do
      code <- readCode mFilename
      let filename = fromMaybe "<stdin>" mFilename
      either (throwM . P.ParserException) pure $
        parse P.program filename code

    -- Read and parse the contract, expand and type check.
    prepareContract
      :: Maybe FilePath -> IO (Contract Op)
    prepareContract mFile =
      expandFlattenContract <$> readAndParseContract mFile
