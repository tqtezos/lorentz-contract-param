{-# LANGUAGE OverloadedStrings #-}
module Language.Michelson.Parser where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Sequence
import           Data.Char as Char
import qualified Data.Text                        as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO                     as TIO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

import qualified Language.Michelson.Types         as M

import Data.Natural
import           Control.Applicative.Permutations
import qualified Data.Sequence                    as Seq
import           Data.Void                        (Void)

type Parser = Parsec Void T.Text

parseFile :: String -> IO M.SC
parseFile file = do
  code <- TIO.readFile file
  case parse smart_contract file code of
    Left e   -> print e >> fail "error"
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
  m_space
  (p,s,c) <- runPermutation $
              (,,) <$> toPermutation param
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ M.SC p s c

param :: Parser M.Param
param = do symbol "parameter"; a <- m_type; sem; return $ M.Param a

storage :: Parser M.Storage
storage = do symbol "storage"; a <- m_type; sem; return $ M.Storage a

code :: Parser M.Code
code = do symbol "code"; a <- iseq; sem; return $ M.Code a

-- Data
m_data :: Parser M.D
m_data = lexeme $ (m_data' <|> parens m_data')

m_data' :: Parser M.D
m_data' = (try $ M.LInt <$> l_int)
      <|> (try $ M.LString <$> l_string)
      <|> (try $ M.LBytes <$> l_bytes)
      <|> (do symbol "Unit"; return M.DUnit)
      <|> (do symbol "True"; return M.DTrue)
      <|> (do symbol "False"; return M.DFalse)
      <|> (do symbol "Pair"; a <- m_data; b <- m_data; return $ M.DPair a b)
      <|> (do symbol "Left"; a <- m_data; return $ M.DLeft a)
      <|> (do symbol "Right"; a <- m_data; return $ M.DRight a)
      <|> (do symbol "Some"; a <- m_data; return $ M.DSome a)
      <|> (do symbol "None"; return M.DNone)
      <|> (try $ M.DSeq <$> d_list)
      <|> (try $ M.DMap <$> d_map)
      <|> (M.DInst <$> iseq)

l_int :: Parser Integer
l_int = (L.signed (return ()) L.decimal)

l_bytes :: Parser B.ByteString
l_bytes = do
  symbol "0x"
  hexdigits <- takeWhile1P Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return bytes
  else (error "odd number bytes")

-- this parses more escape sequences than are in the michelson spec
-- should investigate which sequences this matters for, e.g. \xa == \n
l_string :: Parser T.Text
l_string = T.pack <$> (char '"' >> (manyTill L.charLiteral (char '"')))

{-
-- could do something explicit based on this
strEscape :: Parser T.Text
strEscape = char '\\' >> esc
  where
    esc = (char 'n' >> return "\n")
      <|> (char 't' >> return "\t")
      <|> (char 'b' >> return "\b")
      <|> (char '\\' >> return "\\")
      <|> (char '"' >> return "\"")
-}


d_list :: Parser (Seq M.D)
d_list = Seq.fromList <$> (braces $ sepEndBy m_data sem)

d_elt :: Parser M.Elt
d_elt = do symbol "Elt"; key <- m_data; val <- m_data; return $ M.Elt key val

d_map :: Parser (Seq M.Elt)
d_map = Seq.fromList <$> (braces $ sepEndBy d_elt sem)

-- Instructions
iseq :: Parser M.ISeq
iseq = M.ISeq . Seq.fromList <$> (braces $ sepEndBy instruction sem)

instruction :: Parser M.I
instruction = (symbol "DROP" >> return M.DROP)
    <|> (do symbol "DUP"; return M.DUP)
    <|> (do symbol "SWAP"; return M.SWAP)
    <|> (do symbol "PUSH"; a <- m_type; b <- m_data; return $ M.PUSH a b)
    <|> (do symbol "SOME"; return M.SOME)
    <|> (do symbol "NONE"; a <- m_type; return $ M.NONE a)
    <|> (do symbol "UNIT"; return M.UNIT)
    <|> (do symbol "IF_NONE"; a <- iseq; b <- iseq; return $ M.IF_NONE a b)
    <|> (do symbol "PAIR"; return M.PAIR)
    <|> (do symbol "CAR"; return M.CAR)
    <|> (do symbol "CDR"; return M.CDR)
    <|> (do symbol "LEFT"; a <- m_type; return $ M.LEFT a)
    <|> (do symbol "RIGHT"; a <- m_type; return $ M.RIGHT a)
    <|> (do symbol "NIL"; a <- m_type; return $ M.NIL a)
    <|> (do symbol "CONS"; return M.CONS)
    <|> (do symbol "IF_CONS"; a <- iseq; b <- iseq; return $ M.IF_CONS a b)
    <|> (do symbol "EMPTY_SET"; a <- m_ct; return $ M.EMPTY_SET a)
    <|> (do symbol "EMPTY_MAP"; a <- m_ct; b <- m_type; return $ M.EMPTY_SET a)
    <|> (do symbol "MAP"; a <- iseq; return $ M.MAP a)
    <|> (do symbol "ITER"; a <- iseq; return $ M.ITER a)
    <|> (do symbol "MEM"; return M.MEM)
    <|> (do symbol "GET"; return M.GET)
    <|> (do symbol "UPDATE"; return M.UPDATE)
    <|> (do symbol "IF"; a <- iseq; b <- iseq; return $ M.IF a b)
    <|> (do symbol "LOOP"; a <- iseq; return $ M.LOOP a)
    <|> (do symbol "LOOP_LEFT"; a <- iseq; return $ M.LOOP_LEFT a)
    <|> (do symbol "EXEC"; return M.EXEC)
    <|> (do symbol "DIP"; a <- iseq; return $ M.DIP a)
    <|> (do symbol "FAILWITH"; a <- m_data; return $ M.FAILWITH a)
    <|> (do symbol "CAST"; return M.CAST)
    <|> (do symbol "RENAME"; return M.RENAME)
    <|> (do symbol "CONCAT"; return M.CONCAT)
    <|> (do symbol "ADD"; return M.ADD)
    <|> (do symbol "SUB"; return M.SUB)
    <|> (do symbol "MUL"; return M.MUL)
    <|> (do symbol "DIV"; return M.DIV)
    <|> (do symbol "ABS"; return M.ABS)
    <|> (do symbol "NEG"; return M.NEG)
    <|> (do symbol "MOD"; return M.MOD)
    <|> (do symbol "LSL"; return M.LSL)
    <|> (do symbol "LSR"; return M.LSR)
    <|> (do symbol "OR"; return M.OR)
    <|> (do symbol "AND"; return M.AND)
    <|> (do symbol "NOT"; return M.NOT)
    <|> (do symbol "COMPARE"; return M.COMPARE)
    <|> (do symbol "EQ"; return M.EQ)
    <|> (do symbol "NEQ"; return M.NEQ)
    <|> (do symbol "LT"; return M.LT)
    <|> (do symbol "GT"; return M.GT)
    <|> (do symbol "LE"; return M.LE)
    <|> (do symbol "GE"; return M.GE)
    <|> (do symbol "INT"; return M.INT)
    <|> (do symbol "SELF"; return M.SELF)
    <|> (do symbol "TRANSFER_TOKENS"; return M.TRANSFER_TOKENS)
    <|> (do symbol "SET_DELEGATE"; return M.SET_DELEGATE)
    <|> (do symbol "CREATE_ACCOUNT"; return M.CREATE_ACCOUNT)
    <|> (do symbol "CREATE_CONTRACT"; a <- iseq; return $ M.CREATE_CONTRACT2 a)
    <|> (do symbol "CREATE_CONTRACT"; return M.CREATE_CONTRACT)
    <|> (do symbol "IMPLICIT_ACCOUNT"; return M.IMPLICIT_ACCOUNT)
    <|> (do symbol "NOW"; return M.NOW)
    <|> (do symbol "AMOUNT"; return M.AMOUNT)
    <|> (do symbol "BALANCE"; return M.BALANCE)
    <|> (do symbol "CHECK_SIGNATURE"; return M.CHECK_SIGNATURE)
    <|> (do symbol "BLAKE2B"; return M.BLAKE2B)
    <|> (do symbol "HASH_KEY"; return M.HASH_KEY)
    <|> (do symbol "STEPS_TO_QUOTA"; return M.STEPS_TO_QUOTA)
    <|> (do symbol "SOURCE"; return M.SOURCE)
    <|> (do symbol "SENDER"; return M.SENDER)
    <|> (do symbol "ADDRESS"; return M.ADDRESS)

-- Types
m_type :: Parser M.T
m_type = (M.T_comparable <$> m_ct)
  <|> (do symbol "key"; return M.T_key)
  <|> (do symbol "unit"; return M.T_unit)
  <|> (do symbol "signature"; return M.T_signature)
  <|> (do symbol "option"; a <- m_type; return $ M.T_option a)
  <|> (do symbol "list"; a <- m_type; return $ M.T_list a)
  <|> (do symbol "set"; a <- m_ct; return $ M.T_set a)
  <|> (do symbol "operation"; return $ M.T_operation)
  <|> (do symbol "contract"; a <- m_type; return $ M.T_contract a)
  <|> (do symbol "pair"; a <- m_type; b <- m_type; return $ M.T_pair a b)
  <|> (do symbol "or"; a <- m_type; b <- m_type; return $ M.T_or a b)
  <|> (do symbol "lambda"; a <- m_type; b <- m_type; return $ M.T_lambda a b)
  <|> (do symbol "map"; a <- m_ct; b <- m_type; return $ M.T_map a b)
  <|> (do symbol "big_map"; a <- m_ct; b <- m_type; return $ M.T_big_map a b)
  <|> (parens m_type)

-- Comparable Types
m_ct :: Parser M.CT
m_ct = (symbol "int" >> return M.T_int)
 <|> (symbol "nat" >> return M.T_nat)
 <|> (symbol "string" >> return M.T_string)
 <|> (symbol "mutez" >> return M.T_mutez)
 <|> (symbol "bool" >> return M.T_bool)
 <|> (symbol "key_hash" >> return M.T_key_hash)
 <|> (symbol "timestamp" >> return M.T_timestamp)
