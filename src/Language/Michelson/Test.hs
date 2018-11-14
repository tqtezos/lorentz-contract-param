module Language.Michelson.Test where

import qualified Language.Michelson.Types         as M
import qualified Language.Michelson.Parser        as P
import qualified Data.Text.IO                     as TIO
import System.Directory
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer    as L

parseFile :: FilePath -> IO M.Contract
parseFile file = do
  code <- TIO.readFile file
  case parse P.contract file code of
    Left e   -> print e >> error "fail"
    Right sc -> return sc

--emptyContract = M.Contract (M.Parameter $ M.T_unit M.noTN)
--               (M.Storage $ M.T_unit M.noTN)
--               (M.Code M.noOps)

checkFile :: FilePath -> IO ()
checkFile file = do
  code <- TIO.readFile file
  putStr $ file
  putStr $ replicate (40 - (length file)) ' '
  case parse P.contract file code of
    Left e   -> putStrLn "FAIL"
    Right sc -> putStrLn "SUCCESS"


parseFiles :: FilePath -> IO ()
parseFiles dir = do
  files <- (fmap . fmap) (\s -> dir ++ s) $ listDirectory dir
  traverse checkFile files
  return ()

