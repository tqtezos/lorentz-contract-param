module Language.Michelson.Test
  ( parseFile
  , checkFile
  , parseFiles
  ) where

import qualified Data.Text.IO as TIO
import qualified Language.Michelson.Parser as P
import qualified Language.Michelson.Types as M
import System.Directory
import Text.Megaparsec

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
  putStr file
  putStr $ replicate (40 - length file) ' '
  case parse P.contract file code of
    Left _  -> putTextLn "FAIL" >> return False
    Right _ -> putTextLn "SUCCESS" >> return True


parseFiles :: FilePath -> IO ()
parseFiles dir = do
  files <- (fmap . fmap) (\s -> dir ++ s) $ listDirectory dir
  a <- newIORef @_ @Word 0
  b <- newIORef @_ @Word 0
  let check file ref1 ref2 = do
        s <- checkFile file
        if s
        then modifyIORef ref1 (+1) >> modifyIORef ref2 (+1)
        else modifyIORef ref2 (+1)
  traverse (\f -> check f a b) files
  a' <- readIORef a
  b' <- readIORef b
  putText "Passing "
  print a'
  putText "out of "
  print b'
  return ()
