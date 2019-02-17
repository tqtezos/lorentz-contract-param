module Main
  ( main
  ) where


import Data.Text.IO (getContents)
import Fmt (pretty)
import System.Console.ArgParser
import System.Console.ArgParser.Params (FlagParam, StdArgParam)
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)

import Michelson.Typecheck (typecheckContract)
import Michelson.Types
import Morley.Macro (expandContractMacros, expandFlattenContract, expandValue)
import qualified Morley.Parser as P
import Morley.Runtime (Account(..), TxData(..), originateContract, runContract)
import Morley.Types

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
  }

data OriginateOptions = OriginateOptions
  { ooContractFile :: !(Maybe FilePath)
  , ooDBPath :: !FilePath
  , ooStorageValue :: !(Value Op)
  , ooBalance :: !Mutez
  , ooVerbose :: !Bool
  }

argParser :: IO (CmdLnInterface CmdLnArgs)
argParser = mkSubParser
  [ ("parse", mkDefaultApp
      (Parse `parsedBy` contractFileOption `andBy` boolFlag "expand-macros") "parse")
  , ("typecheck", mkDefaultApp
      (TypeCheck `parsedBy` contractFileOption `andBy` verboseFlag) "typecheck")
  , ("run", mkDefaultApp
      (Run `parsedBy` runOptionsSpec) "run")
  , ("originate", mkDefaultApp
      (Originate `parsedBy` originateOptionsSpec) "originate")
  ]

-- FIXME: stupid `argparser`!!!
-- `StdArgParam` is not a `Functor`!
-- Is it even possible to parse an optional value without supplying a default?!
-- Most likely should stop using it.
contractFileOption :: ParserSpec (Maybe FilePath)
contractFileOption = Just `parsedBy` reqPos "contract"

dbPathOption :: StdArgParam FilePath
dbPathOption = optFlag "db.json" "db"

valueOption :: String -> ParserSpec (Value Op)
valueOption name = mkValue `parsedBy` reqFlag name
  where
    mkValue :: String -> Value Op
    mkValue =
      either (error . mappend "Failed to parse value: ") id .
      parseValue . toText

    parseValue :: Text -> Either Text (Value Op)
    parseValue text =
      either (Left . show) (Right . expandValue) $ parse P.value "" text

mutezOption :: String -> ParserSpec Mutez
mutezOption name = Mutez . fromIntegral `parsedBy` reqFlag @Int name

verboseFlag :: FlagParam Bool
verboseFlag = boolFlag "verbose"

runOptionsSpec :: ParserSpec RunOptions
runOptionsSpec =
  RunOptions `parsedBy`
    contractFileOption `andBy`
    dbPathOption `andBy`
    valueOption "storage" `andBy`
    txDataSpec `andBy`
    verboseFlag

txDataSpec :: ParserSpec TxData
txDataSpec =
  mkTxData `parsedBy`
    reqFlag "sender" `andBy`
    valueOption "parameter" `andBy`
    mutezOption "amount"
  where
    mkTxData :: String -> Value Op -> Mutez -> TxData
    mkTxData addr param amount =
      TxData
        { tdSenderAddress = Address (toText addr)
        -- FIXME: how to properly report error using `argparser`?
        , tdParameter = param
        , tdAmount = amount
        }

originateOptionsSpec :: ParserSpec OriginateOptions
originateOptionsSpec =
  OriginateOptions `parsedBy`
    contractFileOption `andBy`
    dbPathOption `andBy`
    valueOption "storage" `andBy`
    mutezOption "balance" `andBy`
    verboseFlag

main :: IO ()
main = do
  interface <- argParser
  runApp interface run
  where
    run :: CmdLnArgs -> IO ()
    run args = case args of
      Parse mFilename hasExpandMacros -> do
        contract <- readAndParseContract mFilename
        if hasExpandMacros
          then pPrint $ expandContractMacros contract
          else pPrint contract
      TypeCheck mFilename _hasVerboseFlag -> do
        void $ prepareContract mFilename
        putTextLn "Contract is well-typed"
      Run RunOptions {..} -> do
        michelsonContract <- prepareContract roContractFile
        -- TODO: [TM-18] Pass timestamp from CLI if it's provided.
        runContract Nothing roVerbose roDBPath roStorageValue michelsonContract roTxData
      Originate OriginateOptions {..} -> do
        michelsonContract <- prepareContract ooContractFile
        let acc = Account
              { accBalance = ooBalance
              , accStorage = ooStorageValue
              , accContract = michelsonContract
              }
        addr <- originateContract ooVerbose ooDBPath acc
        putTextLn $ "Originated contract " <> pretty addr

    readCode :: Maybe FilePath -> IO Text
    readCode = maybe getContents readFile

    readAndParseContract :: Maybe FilePath -> IO (Contract ParsedOp)
    readAndParseContract mFilename = do
      code <- readCode mFilename
      let filename = fromMaybe "<stdin>" mFilename
      either (throwM . P.ParserException) pure $
        parse P.contract filename code

    -- Read and parse the contract, expand and type check.
    prepareContract :: Maybe FilePath -> IO (Contract Op)
    prepareContract mFile = do
      contract <- readAndParseContract mFile
      let
        michelsonContract :: Contract Op
        michelsonContract = expandFlattenContract contract

      either throwM pure $ typecheckContract michelsonContract
      pure michelsonContract
