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
  , StringLiteralParserException (..)

  -- * Additional helpers
  , parseNoEnv

  -- * For tests
  , codeEntry
  , type_
  , explicitType
  , letType
  , stringLiteral
  , bytesLiteral
  , intLiteral
  , printComment
  ) where

import Prelude hiding (try)

import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import Text.Megaparsec (Parsec, choice, eitherP, getSourcePos, parse, try)
import Text.Megaparsec.Pos (SourcePos(..), unPos)

import Michelson.ErrorPos (SrcPos(..), mkPos)
import Michelson.Macro (LetMacro, Macro(..), ParsedInstr, ParsedOp(..), ParsedValue)
import Michelson.Parser.Error
import Michelson.Parser.Ext
import Michelson.Parser.Instr
import Michelson.Parser.Let
import Michelson.Parser.Lexer
import Michelson.Parser.Macro
import Michelson.Parser.Type
import Michelson.Parser.Types
import Michelson.Parser.Value
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
    parameter = symbol "parameter" *> explicitType

    storage :: Parser Type
    storage = symbol "storage" *> explicitType

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
  pos <- getSrcPos
  choice
    [ flip Prim pos <$> (EXT <$> extInstr ops)
    , lmacWithPos (mkLetMac lms)
    , flip Prim pos <$> prim
    , flip Mac pos <$> macro parsedOp
    , primOrMac
    , flip Seq pos <$> ops
    ]
  where
    lmacWithPos :: Parser LetMacro -> Parser ParsedOp
    lmacWithPos act = do
      srcPos <- getSrcPos
      flip LMac srcPos <$> act

getSrcPos :: Parser SrcPos
getSrcPos = do
  sp <- getSourcePos
  let l = unPos $ sourceLine sp
  let c = unPos $ sourceColumn sp
  -- reindexing starting from 0
  pure $ SrcPos (mkPos $ l - 1) (mkPos $ c - 1)

primWithPos :: Parser ParsedInstr -> Parser ParsedOp
primWithPos act = do
  srcPos <- getSrcPos
  flip Prim srcPos <$> act

macWithPos :: Parser Macro -> Parser ParsedOp
macWithPos act = do
  srcPos <- getSrcPos
  flip Mac srcPos <$> act

ops :: Parser [ParsedOp]
ops = ops' parsedOp

-------------------------------------------------------------------------------
-- Mixed parsers
-- These are needed for better error messages
-------------------------------------------------------------------------------

ifOrIfX :: Parser ParsedOp
ifOrIfX = do
  pos <- getSrcPos
  symbol' "IF"
  a <- eitherP cmpOp ops
  case a of
    Left cmp -> flip Mac pos <$> (IFX cmp <$> ops <*> ops)
    Right op -> flip Prim pos <$> (IF op <$> ops)

-- Some of the operations and macros have the same prefixes in their names
-- So this case should be handled separately
primOrMac :: Parser ParsedOp
primOrMac = (macWithPos (ifCmpMac parsedOp) <|> ifOrIfX)
  <|> (macWithPos (mapCadrMac parsedOp) <|> primWithPos (mapOp parsedOp))
  <|> (try (primWithPos pairOp) <|> macWithPos pairMac)
