module Language.Michelson.Test where

import qualified Language.Michelson.Types         as M
import qualified Language.Michelson.Parser        as P
import qualified Data.Text.IO                     as TIO
import System.Directory
import Data.IORef
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

checkFile :: FilePath -> IO Bool
checkFile file = do
  code <- TIO.readFile file
  putStr $ file
  putStr $ replicate (40 - (length file)) ' '
  case parse P.contract file code of
    Left e   -> putStrLn "FAIL" >> return False
    Right sc -> putStrLn "SUCCESS" >> return True


parseFiles :: FilePath -> IO ()
parseFiles dir = do
  files <- (fmap . fmap) (\s -> dir ++ s) $ listDirectory dir
  a <- newIORef 0
  b <- newIORef 0
  let check file ref1 ref2 = do
        s <- checkFile file
        if s
        then modifyIORef ref1 (+1) >> modifyIORef ref2 (+1)
        else modifyIORef ref2 (+1)
  traverse (\f -> check f a b) files
  a' <- readIORef a
  b' <- readIORef b
  putStr "Passing "
  putStrLn $ show a'
  putStr "out of "
  putStrLn $ show b'
  return ()

