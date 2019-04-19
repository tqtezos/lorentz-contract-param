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
import Michelson.Types (ParsedOp(..))
import qualified Michelson.Types as Mi
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
contract :: Parser (Mi.Contract' ParsedOp)
contract = do
  mSpace
  (p,s,c) <- intercalateEffect semicolon $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ Mi.Contract p s c
  where
    parameter :: Parser Type
    parameter = symbol "parameter" *> type_

    storage :: Parser Type
    storage = symbol "storage" *> type_

    code :: Parser [ParsedOp]
    code = symbol "code" *> codeEntry


-- Value
------------------

value :: Parser Mi.ParsedValue
value = value' parsedOp


-- Primitive instruction
------------------

prim :: Parser Mi.ParsedInstr
prim = primInstr contract parsedOp

-- Parsed operations (primitive instructions, macros, extras, etc.)
------------------

-- | Parses code block after "code" keyword of a contract.
--
-- This function is part of the module API, its semantics should not change.
codeEntry :: Parser [ParsedOp]
codeEntry = ops

parsedOp :: Parser Mi.ParsedOp
parsedOp = do
  lms <- asks letMacros
  choice
    [ (Mi.Prim . Mi.EXT) <$> extInstr ops
    , Mi.LMac <$> mkLetMac lms
    , Mi.Prim <$> prim
    , Mi.Mac <$> macro parsedOp
    , primOrMac
    , Mi.Seq <$> ops
    ]

ops :: Parser [Mi.ParsedOp]
ops = ops' parsedOp

-------------------------------------------------------------------------------
-- Mixed parsers
-- These are needed for better error messages
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
primOrMac = ((Mi.Mac <$> ifCmpMac parsedOp) <|> ifOrIfX)
  <|> ((Mi.Mac <$> mapCadrMac parsedOp) <|> (Mi.Prim <$> mapOp parsedOp))
  <|> (try (Mi.Prim <$> pairOp) <|> Mi.Mac <$> pairMac)
