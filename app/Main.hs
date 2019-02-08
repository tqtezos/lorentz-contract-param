module Main
  ( main
  ) where


import Data.Text.IO (getContents)
import System.Console.ArgParser
import System.Console.ArgParser.Params (FlagParam)
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)

import Michelson.Types
import Morley.Macro (expandContractMacros, expandFlattenContract)
import qualified Morley.Parser as P
import Morley.Runtime (TxData(..), interpreter)
import Morley.Types

data CmdLnArgs
  = Parse (Maybe FilePath) Bool
  | TypeCheck (Maybe FilePath) Bool
  | Run !RunOptions

data RunOptions = RunOptions
  { roContractFile :: !(Maybe FilePath)
  , roDBPath :: !FilePath
  , roTxData :: !TxData
  , roVerbose :: !Bool
  }

argParser :: IO (CmdLnInterface CmdLnArgs)
argParser = mkSubParser
  [ ("parse", mkDefaultApp
      (Parse `parsedBy` contractFileOption `andBy` boolFlag "expand-macros") "parse")
  , ("typecheck", mkDefaultApp
      (TypeCheck `parsedBy` contractFileOption `andBy` verboseFlag) "typecheck")
  , ("run", mkDefaultApp
      (Run `parsedBy` runOptionsSpec) "run")
  ]

-- FIXME: stupid `argparser`!!!
-- `StdArgParam` is not a `Functor`!
-- Is it even possible to parse an optional value without supplying a default?!
-- Most likely should stop using it.
contractFileOption :: ParserSpec (Maybe FilePath)
contractFileOption = Just `parsedBy` reqPos "contract"

verboseFlag :: FlagParam Bool
verboseFlag = boolFlag "verbose"

runOptionsSpec :: ParserSpec RunOptions
runOptionsSpec =
  RunOptions `parsedBy`
    contractFileOption `andBy`
    optFlag "db.json" "db" `andBy`
    txDataSpec `andBy`
    verboseFlag

txDataSpec :: ParserSpec TxData
txDataSpec =
  mkTxData `parsedBy`
    reqFlag "sender" `andBy`
    reqFlag "parameter" `andBy`
    reqFlag "amount"
  where
    mkTxData :: String -> String -> Int -> TxData
    mkTxData addr param amount =
      TxData
        { tdSenderAddress = Address (toText addr)
        -- FIXME: how to properly report error using `argparser`?
        , tdParameter =
            either (error . mappend "Failed to parse parameter: ") id $
            parseValue (toText param)
        , tdAmount = Mutez (fromIntegral amount)
        }

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
      TypeCheck _filename _hasVerboseFlag -> error "Not implemented yet:("
      Run ro -> runInterpreter ro

    readCode :: Maybe FilePath -> IO Text
    readCode = maybe getContents readFile

    readAndParseContract :: Maybe FilePath -> IO (Contract ParsedOp)
    readAndParseContract mFilename = do
      code <- readCode mFilename
      let filename = fromMaybe "<stdin>" mFilename
      either (throwM . P.ParserException) pure $
        parse P.contract filename code

    runInterpreter :: RunOptions -> IO ()
    runInterpreter RunOptions {..} = do
      contract <- readAndParseContract roContractFile
      let
        michelsonContract :: Contract Op
        michelsonContract = expandFlattenContract contract

      -- TODO: call type checker here!
      -- TODO: [TM-18] Pass timestamp from CLI if it's provided.
      interpreter Nothing roVerbose roDBPath michelsonContract roTxData
