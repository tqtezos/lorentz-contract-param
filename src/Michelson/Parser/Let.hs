-- | Parsing of let blocks

module Michelson.Parser.Let
  ( letBlock
  , mkLetMac
  ) where

import Prelude hiding (try)

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Megaparsec (choice, satisfy, try)
import Text.Megaparsec.Char (lowerChar, upperChar)

import Michelson.Lexer
import Michelson.Parser.Ext
import Michelson.Parser.Helpers
import Michelson.Parser.Instr
import Michelson.Parser.Type
import Michelson.Parser.Types (LetEnv(..), Parser, noLetEnv)
import Michelson.Parser.Value
import Michelson.Types (ParsedOp(..))
import qualified Michelson.Types as Mi

-- | Element of a let block
data Let = LetM Mi.LetMacro | LetV Mi.LetValue | LetT Mi.LetType

-- | let block parser
letBlock :: Parser ParsedOp -> Parser LetEnv
letBlock opParser = do
  symbol "let"
  symbol "{"
  ls <- local (const noLetEnv) (letInner opParser)
  symbol "}"
  semicolon
  return ls

-- | Incrementally build the let environment
letInner :: Parser ParsedOp -> Parser LetEnv
letInner opParser = do
  env <- ask
  l <- lets opParser
  semicolon
  local (addLet l) (letInner opParser) <|> return (addLet l env)

-- | add a Let to the environment in the correct place
addLet :: Let -> LetEnv -> LetEnv
addLet l (LetEnv lms lvs lts) = case l of
  LetM lm -> LetEnv (Map.insert (Mi.lmName lm) lm lms) lvs lts
  LetV lv -> LetEnv lms (Map.insert (Mi.lvName lv) lv lvs) lts
  LetT lt -> LetEnv lms lvs (Map.insert (Mi.ltName lt) lt lts)

lets :: Parser ParsedOp -> Parser Let
lets opParser = choice
  [ (LetM <$> (try $ letMacro opParser))
  , (LetV <$> (try $ letValue opParser))
  , (LetT <$> (try letType))
  ]

-- | build a let name parser from a leading character parser
letName :: Parser Char -> Parser Text
letName p = lexeme $ do
  v <- p
  let validChar x = Char.isAscii x && (Char.isAlphaNum x || x == '\'' || x == '_')
  vs <- many (satisfy validChar)
  return $ toText (v:vs)

letMacro :: Parser ParsedOp -> Parser Mi.LetMacro
letMacro opParser = lexeme $ do
  n <- letName lowerChar
  symbol "::"
  s <- stackFn
  symbol "="
  o <- ops' opParser
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

letValue :: Parser ParsedOp -> Parser Mi.LetValue
letValue opParser = lexeme $ do
  n <- letName upperChar
  symbol "::"
  t <- type_
  symbol "="
  v <- value' opParser
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
