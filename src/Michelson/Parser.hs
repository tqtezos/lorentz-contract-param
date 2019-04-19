module Michelson.Parser
  ( program
  , parseNoEnv
  , codeEntry
  , type_
  , value
  , stackType
  , printComment

  -- * Errors
  , CustomParserException (..)
  , ParseErrorBundle
  , ParserException (..)

  -- * For tests
  , stringLiteral
  , bytesLiteral
  , intLiteral
  ) where

import Prelude hiding (many, note, try)

import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Text.Megaparsec (choice, eitherP, many, parse, satisfy, try)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Michelson.Lexer
import Michelson.Parser.Error
import Michelson.Parser.Helpers
import Michelson.Parser.Instr
import Michelson.Parser.Macro
import Michelson.Parser.Type
import Michelson.Parser.Value
import Michelson.Types (ParsedOp(..), Parser)
import qualified Michelson.Types as Mi
import qualified Michelson.Untyped as U

-------------------------------------------------------------------------------
-- Top-Level Parsers
-------------------------------------------------------------------------------


-- Contracts
------------------

-- | Michelson contract with let definitions
program :: Mi.Parsec CustomParserException T.Text (Mi.Contract' ParsedOp)
program = runReaderT programInner Mi.noLetEnv

programInner :: Parser (Mi.Contract' ParsedOp)
programInner = do
  mSpace
  env <- fromMaybe Mi.noLetEnv <$> (optional letBlock)
  local (const env) contract

-- | Parse with empty environment
parseNoEnv ::
     Parser a
  -> String
  -> Text
  -> Either (ParseErrorBundle Text CustomParserException) a
parseNoEnv p = parse (runReaderT p Mi.noLetEnv)

-- | Michelson contract
contract :: Parser (Mi.Contract' ParsedOp)
contract = do
  mSpace
  (p,s,c) <- intercalateEffect semicolon $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ Mi.Contract p s c

-- Value
------------------

value :: Parser Mi.ParsedValue
value = value' op'


-- Primitive instruction
------------------

prim :: Parser Mi.ParsedInstr
prim = primInstr contract op'

-- Contract Blocks
------------------

-- | let block parser
letBlock :: Parser Mi.LetEnv
letBlock = do
  symbol "let"
  symbol "{"
  ls <- local (const Mi.noLetEnv) letInner
  symbol "}"
  semicolon
  return ls

parameter :: Parser Mi.Type
parameter = do void $ symbol "parameter"; type_

storage :: Parser Mi.Type
storage = do void $ symbol "storage"; type_

code :: Parser [ParsedOp]
code = do void $ symbol "code"; codeEntry

-- Michelson expressions
------------------------
-- | Parses code block after "code" keyword of a contract.
--
-- This function is part of the module API, its semantics should not change.
codeEntry :: Parser [ParsedOp]
codeEntry = ops

op' :: Parser Mi.ParsedOp
op' = do
  lms <- asks Mi.letMacros
  choice
    [ (Mi.Prim . Mi.EXT) <$> nopInstr
    , Mi.LMac <$> mkLetMac lms
    , Mi.Prim <$> prim
    , Mi.Mac <$> macro op'
    , primOrMac
    , Mi.Seq <$> ops
    ]

ops :: Parser [Mi.ParsedOp]
ops = ops' op'

-------------------------------------------------------------------------------
-- Let block
-------------------------------------------------------------------------------

-- | Element of a let block
data Let = LetM Mi.LetMacro | LetV Mi.LetValue | LetT Mi.LetType

-- | Incrementally build the let environment
letInner :: Parser Mi.LetEnv
letInner = do
  env <- ask
  l <- lets
  semicolon
  (local (addLet l) letInner) <|> return (addLet l env)

-- | add a Let to the environment in the correct place
addLet :: Let -> Mi.LetEnv -> Mi.LetEnv
addLet l (Mi.LetEnv lms lvs lts) = case l of
  LetM lm -> Mi.LetEnv (Map.insert (Mi.lmName lm) lm lms) lvs lts
  LetV lv -> Mi.LetEnv lms (Map.insert (Mi.lvName lv) lv lvs) lts
  LetT lt -> Mi.LetEnv lms lvs (Map.insert (Mi.ltName lt) lt lts)

lets :: Parser Let
lets = choice [ (LetM <$> (try letMacro))
              , (LetV <$> (try letValue))
              , (LetT <$> (try letType))
              ]

-- | build a let name parser from a leading character parser
letName :: Parser Char -> Parser T.Text
letName p = lexeme $ do
  v <- p
  let validChar x = Char.isAscii x && (Char.isAlphaNum x || x == '\'' || x == '_')
  vs <- many (satisfy validChar)
  return $ T.pack (v:vs)

letMacro :: Parser Mi.LetMacro
letMacro = lexeme $ do
  n <- letName lowerChar
  symbol "::"
  s <- stackFn
  symbol "="
  o <- ops
  return $ Mi.LetMacro n s o

letType :: Parser Mi.LetType
letType = lexeme $ do
  symbol "type"
  n <- letName upperChar <|> letName lowerChar
  symbol "="
  t <- type_
  case t of
    (Mi.Type t' a) ->
      if a == Mi.noAnn
      then return $ Mi.LetType n (Mi.Type t' (Mi.ann n))
      else return $ Mi.LetType n t

letValue :: Parser Mi.LetValue
letValue = lexeme $ do
  n <- letName upperChar
  symbol "::"
  t <- type_
  symbol "="
  v <- value
  return $ Mi.LetValue n t v

mkLetMac :: Map Text Mi.LetMacro -> Parser Mi.LetMacro
mkLetMac lms = choice $ mkParser Mi.lmName <$> (Map.elems lms)

stackFn :: Parser Mi.StackFn
stackFn = do
  vs <- (optional (symbol "forall" >> some varID <* symbol "."))
  a <- stackType
  symbol "->"
  b <- stackType
  return $ Mi.StackFn (Set.fromList <$> vs) a b

tyVar :: Parser Mi.TyVar
tyVar = (Mi.TyCon <$> type_) <|> (Mi.VarID <$> varID)

lowerAlphaNumChar :: Parser Char
lowerAlphaNumChar = satisfy (\x -> Char.isLower x || Char.isDigit x)

varID :: Parser Mi.Var
varID = lexeme $ do
  v <- lowerChar
  vs <- many lowerAlphaNumChar
  return $ Mi.Var (T.pack (v:vs))

-------------------------------------------------------------------------------
-- Macro Parsers
-------------------------------------------------------------------------------
ifOrIfX :: Parser Mi.ParsedOp
ifOrIfX = do
  symbol' "IF"
  a <- eitherP cmpOp ops
  case a of
    Left cmp -> Mi.Mac <$> (Mi.IFX cmp <$> ops <*> ops)
    Right op -> Mi.Prim <$> (Mi.IF op <$> ops)

-- Some of the operations and macros have the same prefixes in their names
-- So this case should be handled separately
primOrMac :: Parser Mi.ParsedOp
primOrMac = ((Mi.Mac <$> ifCmpMac op') <|> ifOrIfX)
  <|> ((Mi.Mac <$> mapCadrMac op') <|> (Mi.Prim <$> mapOp op'))
  <|> (try (Mi.Prim <$> pairOp) <|> Mi.Mac <$> pairMac)

-------------------------------------------------------------------------------
-- Morley Instructions
-------------------------------------------------------------------------------

nopInstr :: Parser Mi.ParsedUExtInstr
nopInstr = choice [stackOp, testAssertOp, printOp]

stackOp :: Parser Mi.ParsedUExtInstr
stackOp = symbol' "STACKTYPE" >> U.STACKTYPE <$> stackType

testAssertOp :: Parser Mi.ParsedUExtInstr
testAssertOp = symbol' "TEST_ASSERT" >> U.UTEST_ASSERT <$> testAssert

printOp :: Parser Mi.ParsedUExtInstr
printOp = symbol' "PRINT" >> U.UPRINT <$> printComment

testAssert :: Parser Mi.ParsedUTestAssert
testAssert = do
  n <- lexeme (T.pack <$> some alphaNumChar)
  c <- printComment
  o <- ops
  return $ U.TestAssert n c o

printComment :: Parser U.PrintComment
printComment = do
  string "\""
  let validChar = T.pack <$> some (satisfy (\x -> x /= '%' && x /= '"'))
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
