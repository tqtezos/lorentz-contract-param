module Michelson.Parser
  ( -- * Main parser type
    Parser

  -- * Parsers
  , program
  , value

  -- * Errors
  , CustomParserException (..)
  , ParseErrorBundle
  , ParserException (..)

  -- * Additional helpers
  , parseNoEnv

  -- * For tests
  , codeEntry
  , type_
  , stringLiteral
  , bytesLiteral
  , intLiteral
  , printComment
  ) where

import Prelude hiding (try)

import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import Text.Megaparsec (Parsec, choice, eitherP, parse, try)

import Michelson.Parser.Error
import Michelson.Parser.Ext
import Michelson.Parser.Instr
import Michelson.Parser.Let
import Michelson.Parser.Lexer
import Michelson.Parser.Macro
import Michelson.Parser.Type
import Michelson.Parser.Types
import Michelson.Parser.Value
import Michelson.Macro (Macro(..), ParsedOp(..), ParsedInstr, ParsedValue)
import Michelson.Untyped

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Parse with empty environment
parseNoEnv ::
     Parser a
  -> String
  -> Text
  -> Either (ParseErrorBundle Text CustomParserException) a
parseNoEnv p = parse (runReaderT p noLetEnv)

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

-- Contract
------------------

-- | Michelson contract with let definitions
program :: Parsec CustomParserException Text (Contract' ParsedOp)
program = runReaderT programInner noLetEnv
  where
    programInner :: Parser (Contract' ParsedOp)
    programInner = do
      mSpace
      env <- fromMaybe noLetEnv <$> (optional (letBlock parsedOp))
      local (const env) contract

-- | Michelson contract
contract :: Parser (Contract' ParsedOp)
contract = do
  mSpace
  (p,s,c) <- intercalateEffect semicolon $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ Contract p s c
  where
    parameter :: Parser Type
    parameter = symbol "parameter" *> type_

    storage :: Parser Type
    storage = symbol "storage" *> type_

    code :: Parser [ParsedOp]
    code = symbol "code" *> codeEntry


-- Value
------------------

value :: Parser ParsedValue
value = value' parsedOp


-- Primitive instruction
------------------

prim :: Parser ParsedInstr
prim = primInstr contract parsedOp

-- Parsed operations (primitive instructions, macros, extras, etc.)
------------------

-- | Parses code block after "code" keyword of a contract.
--
-- This function is part of the module API, its semantics should not change.
codeEntry :: Parser [ParsedOp]
codeEntry = ops

parsedOp :: Parser ParsedOp
parsedOp = do
  lms <- asks letMacros
  choice
    [ (Prim . EXT) <$> extInstr ops
    , LMac <$> mkLetMac lms
    , Prim <$> prim
    , Mac <$> macro parsedOp
    , primOrMac
    , Seq <$> ops
    ]

ops :: Parser [ParsedOp]
ops = ops' parsedOp

-------------------------------------------------------------------------------
-- Mixed parsers
-- These are needed for better error messages
-------------------------------------------------------------------------------

ifOrIfX :: Parser ParsedOp
ifOrIfX = do
  symbol' "IF"
  a <- eitherP cmpOp ops
  case a of
    Left cmp -> Mac <$> (IFX cmp <$> ops <*> ops)
    Right op -> Prim <$> (IF op <$> ops)

-- Some of the operations and macros have the same prefixes in their names
-- So this case should be handled separately
primOrMac :: Parser ParsedOp
primOrMac = ((Mac <$> ifCmpMac parsedOp) <|> ifOrIfX)
  <|> ((Mac <$> mapCadrMac parsedOp) <|> (Prim <$> mapOp parsedOp))
  <|> (try (Prim <$> pairOp) <|> Mac <$> pairMac)
