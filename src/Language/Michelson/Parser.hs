{-# LANGUAGE OverloadedStrings #-}

module Language.Michelson.Parser where

import qualified Data.Text                  as T
import qualified Data.Text.IO as TIO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Language.Michelson.Types   as M

import           Data.Void                (Void)
import qualified Data.Sequence as Seq

type Parser = Parsec Void T.Text

parseFile :: String -> IO M.SC
parseFile file = do
  code <- TIO.readFile file
  case parse smart_contract file code of
    Left e -> print e >> fail "error"
    Right sc -> return sc

emptySC = M.SC (M.Param M.T_unit)
               (M.Storage M.T_unit)
               (M.Code (M.ISeq Seq.Empty))

-- Lexing
lexeme = L.lexeme m_space
m_space = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol = L.symbol m_space
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
sem = symbol ";"

smart_contract :: Parser M.SC
smart_contract = do
  p <- param
  s <- storage
  c <- code
  return $ M.SC p s c

param :: Parser M.Param
param = (symbol "parameter") >> m_type >>= (\a -> sem >> (return $ M.Param a))

storage :: Parser M.Storage
storage = (symbol "storage") >> m_type >>= (\a -> sem >> (return $ M.Storage a))

code :: Parser M.Code
code = (symbol "code") >> iseq >>= (\a -> sem >> (return $ M.Code a))

-- Data
m_data :: Parser M.D
m_data = lexeme $
  d_Unit <|> d_True <|> d_False

-- todo: natural numbers, signed numbers

l_int :: Parser Integer
l_int = L.decimal <|> (string "0x" >> L.hexadecimal)
-- todo: escape sequences

--strChar :: Parser Char
--strChar = oneOf [' '...'

--escapeSeqs :: Parser Char
--escapeSeqs =

l_string :: Parser T.Text
l_string = do
  char '"'
  str <- manyTill L.charLiteral (char '"')
  return $ T.pack str

-- todo: various "string" literals

d_Unit :: Parser M.D
d_Unit = string "Unit" >> return M.DUnit

d_True :: Parser M.D
d_True = string "True" >> return M.DTrue

d_False :: Parser M.D
d_False = string "False" >> return M.DFalse

d_Pair :: Parser M.D
d_Pair = do
  string "Pair"
  a <- m_data
  b <- m_data
  return $ M.DPair a b

d_Left :: Parser M.D
d_Left = do
  string "Left"
  a <- m_data
  return $ M.DLeft a

d_Right :: Parser M.D
d_Right = do
  string "Right"
  a <- m_data
  return $ M.DRight a

d_Some :: Parser M.D
d_Some = do
  string "Some"
  a <- m_data
  return $ M.DSome a

d_None :: Parser M.D
d_None = string "None" >> return M.DNone

-- Instructions

iseq :: Parser M.ISeq
iseq = lexeme $ braces $ M.ISeq . Seq.fromList <$> many instruction

instruction :: Parser M.I
instruction = ins <* sem where
  ins = (symbol "DROP" >> return M.DROP)
    <|> (symbol "DUP" >> return M.DUP)
    <|> (symbol "SWAP" >> return M.SWAP)
    <|> (symbol "PUSH" >> m_type >>= (\a -> m_data >>= (\b ->
          return $ M.PUSH a b)))
    <|> (string "SOME" >> return M.SOME)
    <|> (symbol "NONE" >> m_type >>= (\a -> return $ M.NONE a))
    <|> (symbol "UNIT" >> return M.UNIT)
    <|> (symbol "IF_NONE" >> iseq >>= (\a -> iseq >>= (\b ->
          return $ M.IF_NONE a b)))
    <|> (symbol "PAIR" >> return M.PAIR)
    <|> (symbol "CAR" >> return M.CAR)
    <|> (symbol "CDR" >> return M.CDR)
    <|> (symbol "LEFT" >> m_type >>= (\a -> return $ M.LEFT a))
    <|> (symbol "RIGHT" >> m_type >>= (\a -> return $ M.RIGHT a))
    <|> (symbol "NIL" >> m_type >>= (\a -> return $ M.NIL a))
    <|> (symbol "CONS" >> return M.CONS)
    <|> (symbol "IF_CONS" >> iseq >>= (\a -> iseq >>= (\b ->
          return $ M.IF_CONS a b)))
    <|> (symbol "EMPTY_SET" >> m_cmptype >>= (\a -> return $ M.EMPTY_SET a))
    <|> (symbol "EMPTY_MAP" >> m_cmptype >>= (\a -> m_type >>= (\b ->
          return $ M.EMPTY_SET a)))
    <|> (symbol "MAP" >> iseq >>= \a -> return $ M.MAP a)
    <|> (symbol "ITER" >> iseq >>= \a -> return $ M.ITER a)
    <|> (symbol "MEM" >> return M.MEM)
    <|> (symbol "GET" >> return M.GET)
    <|> (symbol "UPDATE" >> return M.UPDATE)
    <|> (symbol "IF" >> iseq >>= (\a -> iseq >>= (\b ->
          return $ M.IF a b)))
    <|> (symbol "LOOP" >> iseq >>= \a -> return $ M.LOOP a)
    <|> (symbol "LOOP_LEFT" >> iseq >>= \a -> return $ M.LOOP_LEFT a)
    <|> (symbol "EXEC" >> return M.EXEC)
    <|> (symbol "DIP" >> iseq >>= \a -> return $ M.DIP a)
    <|> (symbol "FAILWITH" >> m_data >>= (\a -> return $ M.FAILWITH a))
    <|> (symbol "CAST" >> return M.CAST)
    <|> (symbol "RENAME" >> return M.RENAME)
    <|> (symbol "CONCAT" >> return M.CONCAT)
    <|> (symbol "ADD" >> return M.ADD)
    <|> (symbol "SUB" >> return M.SUB)
    <|> (symbol "MUL" >> return M.MUL)
    <|> (symbol "DIV" >> return M.DIV)
    <|> (symbol "ABS" >> return M.ABS)
    <|> (symbol "NEG" >> return M.NEG)
    <|> (symbol "MOD" >> return M.MOD)
    <|> (symbol "LSL" >> return M.LSL)
    <|> (symbol "LSR" >> return M.LSR)
    <|> (symbol "OR" >> return M.OR)
    <|> (symbol "AND" >> return M.AND)
    <|> (symbol "NOT" >> return M.NOT)
    <|> (symbol "COMPARE" >> return M.COMPARE)
    <|> (symbol "EQ" >> return M.EQ)
    <|> (symbol "NEQ" >> return M.NEQ)
    <|> (symbol "LT" >> return M.LT)
    <|> (symbol "GT" >> return M.GT)
    <|> (symbol "LE" >> return M.LE)
    <|> (symbol "GE" >> return M.GE)
    <|> (symbol "INT" >> return M.INT)
    <|> (symbol "SELF" >> return M.SELF)
    <|> (symbol "TRANSFER_TOKENS" >> return M.TRANSFER_TOKENS)
    <|> (symbol "SET_DELEGATE" >> return M.SET_DELEGATE)
    <|> (symbol "CREATE_ACCOUNT" >> return M.CREATE_ACCOUNT)
    <|> (symbol "CREATE_CONTRACT" >> iseq >>= (\a ->
          return $ M.CREATE_CONTRACT2 a))
    <|> (symbol "CREATE_CONTRACT" >> return M.CREATE_CONTRACT)
    <|> (symbol "IMPLICIT_ACCOUNT" >> return M.IMPLICIT_ACCOUNT)
    <|> (symbol "NOW" >> return M.NOW)
    <|> (symbol "AMOUNT" >> return M.AMOUNT)
    <|> (symbol "BALANCE" >> return M.BALANCE)
    <|> (symbol "CHECK_SIGNATURE" >> return M.CHECK_SIGNATURE)
    <|> (symbol "BLAKE2B" >> return M.BLAKE2B)
    <|> (symbol "HASH_KEY" >> return M.HASH_KEY)
    <|> (symbol "STEPS_TO_QUOTA" >> return M.STEPS_TO_QUOTA)
    <|> (symbol "SOURCE" >> return M.SOURCE)
    <|> (symbol "SENDER" >> return M.SENDER)

-- Types
m_type :: Parser M.T
m_type = lexeme $ (symbol "key" >> return M.T_key)
  <|> (symbol "unit" >> return M.T_unit)
  <|> (symbol "signature" >> return M.T_signature)
  <|> (symbol "option" >> m_type >>= (\a -> return $ M.T_option a))
  <|> (symbol "list" >> m_type >>= (\a -> return $ M.T_list a))
  <|> (symbol "set" >> m_cmptype >>= (\a -> return $ M.T_set a))
  <|> (symbol "contract" >> m_type >>= (\a -> return $ M.T_contract a))
  <|> (symbol "pair" >> m_type >>= (\a -> m_type >>= (\b ->
        return $ M.T_pair a b)))
  <|> (symbol "or" >> m_type >>= (\a -> m_type >>= (\b ->
        return $ M.T_or a b)))
  <|> (symbol "lambda" >> m_type >>= (\a -> m_type >>= (\b ->
        return $ M.T_lambda a b)))
  <|> (symbol "map" >> m_cmptype >>= (\a -> m_type >>= (\b ->
        return $ M.T_map a b)))
  <|> (symbol "big_map" >> m_cmptype >>= (\a -> m_type >>= (\b ->
        return $ M.T_big_map a b)))
  <|> parens m_type

-- Comparable Types
m_cmptype :: Parser M.CT
m_cmptype = lexeme $ (symbol "int" >> return M.T_int)
 <|> (symbol "nat" >> return M.T_nat)
 <|> (symbol "string" >> return M.T_string)
 <|> (symbol "mutez" >> return M.T_mutez)
 <|> (symbol "bool" >> return M.T_bool)
 <|> (symbol "key_hash" >> return M.T_key_hash)
 <|> (symbol "timestamp" >> return M.T_timestamp)
