module Main
  ( main
  ) where

import Data.Text.IO (getContents)
import Language.Michelson.Macro (expandContractMacros)
import qualified Language.Michelson.Parser as P

import System.Console.ArgParser
import Text.Megaparsec (parse)
import Text.Pretty.Simple (pPrint)

data CmdLnArgs
  = Parse String Bool
  | TypeCheck String Bool
  | Run String String String Bool

argParser :: IO (CmdLnInterface CmdLnArgs)
argParser = mkSubParser
  [ ("parse", mkDefaultApp
      (Parse `parsedBy` optPos "stdin" "filename" `andBy` boolFlag "expand-macros") "parse")
  , ("typecheck", mkDefaultApp
      (TypeCheck `parsedBy` optPos "stdin" "filename" `andBy` boolFlag "verbose") "typecheck")
  , ("run", mkDefaultApp
      (Run `parsedBy` optPos "stdin" "filename" `andBy`
        reqFlag "storage" `andBy` reqFlag "input" `andBy` boolFlag "verbose") "run")
  ]

main :: IO ()
main = do
  interface <- argParser
  runApp interface run
  where
    run :: CmdLnArgs -> IO ()
    run args = case args of
      Parse filename hasExpandMacros -> do
        code <- case filename of
          "stdin" -> getContents
          _ -> readFile filename
        case parse P.contract filename code of
          Right contract
            | hasExpandMacros -> pPrint $ expandContractMacros contract
            | otherwise -> pPrint contract
          Left e -> throwM $ P.ParserException e
      TypeCheck _filename _hasVerboseFlag -> error "Not implemented yet:("
      Run _filename _storage _input _hasVerboseFlag -> error "Not implemented yet:("
