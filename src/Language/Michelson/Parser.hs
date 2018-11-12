{-# LANGUAGE OverloadedStrings #-}
module Language.Michelson.Parser where

import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

import qualified Language.Michelson.Types         as M
import qualified Language.Michelson.Parser.Prim   as P
import qualified Language.Michelson.Parser.Macro  as Macro


import           Control.Applicative.Permutations


parseFile :: String -> IO M.Contract
parseFile file = do
  code <- TIO.readFile file
  case parse contract file code of
    Left e   -> print e >> fail "error"
    Right sc -> return sc

--emptyContract = M.Contract (M.Parameter M.T_unit)
--               (M.Storage M.T_unit)
--               (M.Code (M.ISeq Seq.Empty))

contract :: P.Parser M.Contract
contract = do
  P.mSpace
  (p,s,c) <- runPermutation $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ M.Contract p s c


parameter :: P.Parser M.Parameter
parameter = do
  P.symbol "parameter"
  a <- P.type_
  P.semicolon
  return $ M.Parameter a

storage :: P.Parser M.Storage
storage = do
  P.symbol "storage"
  a <- P.type_
  P.semicolon
  return $ M.Storage a

code :: P.Parser M.Code
code = do
  P.symbol "code";
  a <- P.ops
  optional P.semicolon
  return $ M.Code a

