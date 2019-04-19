module Michelson.Parser
  ( -- * Main parser type
    Parser

  -- * Parsers
  , program
  , codeEntry
  , type_
  , value
  , stackType

  -- * Errors
  , CustomParserException (..)
  , ParseErrorBundle
  , ParserException (..)

  -- * Additional helpers
  , parseNoEnv

  -- * For tests
  , stringLiteral
  , bytesLiteral
  , intLiteral
  , printComment
  ) where

import Prelude hiding (many, note, try)

import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import Text.Megaparsec (Parsec, choice, eitherP, parse, try)

import Michelson.Lexer
import Michelson.Parser.Error
import Michelson.Parser.Ext
import Michelson.Parser.Instr
import Michelson.Parser.Let
import Michelson.Parser.Macro
import Michelson.Parser.Type
import Michelson.Parser.Types
import Michelson.Parser.Value
import Michelson.Types (ParsedOp(..))
import qualified Michelson.Types as Mi

-------------------------------------------------------------------------------
-- Top-Level Parsers
-------------------------------------------------------------------------------


-- Contracts
------------------

-- | Michelson contract with let definitions
program :: Parsec CustomParserException Text (Mi.Contract' ParsedOp)
program = runReaderT programInner noLetEnv

programInner :: Parser (Mi.Contract' ParsedOp)
programInner = do
  mSpace
  env <- fromMaybe noLetEnv <$> (optional (letBlock op'))
  local (const env) contract

-- | Parse with empty environment
parseNoEnv ::
     Parser a
  -> String
  -> Text
  -> Either (ParseErrorBundle Text CustomParserException) a
parseNoEnv p = parse (runReaderT p noLetEnv)

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
  lms <- asks letMacros
  choice
    [ (Mi.Prim . Mi.EXT) <$> extInstr ops
    , Mi.LMac <$> mkLetMac lms
    , Mi.Prim <$> prim
    , Mi.Mac <$> macro op'
    , primOrMac
    , Mi.Seq <$> ops
    ]

ops :: Parser [Mi.ParsedOp]
ops = ops' op'

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
