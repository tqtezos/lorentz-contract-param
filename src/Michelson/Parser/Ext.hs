-- | Parsing logic for extra instructions (Morley extensions)

module Michelson.Parser.Ext
  ( extInstr
  , stackType

  -- * For tests
  , printComment
  ) where

import Prelude hiding (try)


import Text.Megaparsec (choice, satisfy, try)
import Text.Megaparsec.Char (alphaNumChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Michelson.Parser.Lexer
import Michelson.Parser.Type
import Michelson.Parser.Types (Parser)
import Michelson.Types (ParsedOp(..), ParsedUExtInstr, ParsedUTestAssert)
import qualified Michelson.Untyped as U

extInstr :: Parser [ParsedOp] -> Parser ParsedUExtInstr
extInstr opsParser = choice [stackOp, testAssertOp opsParser, printOp]

stackOp :: Parser ParsedUExtInstr
stackOp = symbol' "STACKTYPE" >> U.STACKTYPE <$> stackType

testAssertOp :: Parser [ParsedOp] -> Parser ParsedUExtInstr
testAssertOp opsParser =
  symbol' "TEST_ASSERT" >> U.UTEST_ASSERT <$> testAssert opsParser

printOp :: Parser ParsedUExtInstr
printOp = symbol' "PRINT" >> U.UPRINT <$> printComment

testAssert :: Parser [ParsedOp] -> Parser ParsedUTestAssert
testAssert opsParser = do
  n <- lexeme (toText <$> some alphaNumChar)
  c <- printComment
  o <- opsParser
  return $ U.TestAssert n c o

printComment :: Parser U.PrintComment
printComment = do
  string "\""
  let validChar = toText <$> some (satisfy (\x -> x /= '%' && x /= '"'))
  c <- many (Right <$> stackRef <|> Left <$> validChar)
  symbol "\""
  return $ U.PrintComment c

stackRef :: Parser U.StackRef
stackRef = do
  string "%"
  n <- brackets' L.decimal
  return $ U.StackRef n

stackType :: Parser U.StackTypePattern
stackType = symbol "'[" >> (emptyStk <|> stkCons <|> stkRest)
  where
    emptyStk = try $ symbol "]" >> return U.StkEmpty
    stkRest = try $ symbol "..." >> symbol "]" >> return U.StkRest
    stkCons = try $ do
      t <- tyVar
      s <- (symbol "," >> stkCons <|> stkRest) <|> emptyStk
      return $ U.StkCons t s

tyVar :: Parser U.TyVar
tyVar = (U.TyCon <$> type_) <|> (U.VarID <$> varID)
