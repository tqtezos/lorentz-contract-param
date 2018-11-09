{-# LANGUAGE OverloadedStrings #-}
module Language.Michelson.Parser where

import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as B16
import           Data.Char                        as Char
import           Data.Sequence
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Text.IO                     as TIO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

import qualified Language.Michelson.Types         as M

import           Control.Applicative.Permutations
import           Data.Natural
import qualified Data.Sequence                    as Seq
import           Data.Void                        (Void)

type Parser = Parsec Void T.Text

parseFile :: String -> IO M.SC
parseFile file = do
  code <- TIO.readFile file
  case parse smart_contract file code of
    Left e   -> print e >> fail "error"
    Right sc -> return sc

--emptySC = M.SC (M.Param M.T_unit)
--               (M.Storage M.T_unit)
--               (M.Code (M.ISeq Seq.Empty))

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

-- Annotations

ntn = M.Nt Nothing
nfn = M.Nf Nothing
nvn = M.Nv Nothing

nt :: Parser M.Nt
nt = M.Nt <$> (optional $ lexeme $ char ':' >> (note <|> (pure "")))

nf :: Parser M.Nf
nf = M.Nf <$> (optional $ lexeme $ char '%' >> (note <|> (pure "")))

nv :: Parser M.Nv
nv = M.Nv <$> (optional $ lexeme $ char '@' >> (note <|> (pure "")))

note :: Parser T.Text
note = do
  a <- string "@" <|> string "%" <|> string "%%" <|> T.singleton <$> satisfy isAlphaNum
  b <- takeWhileP Nothing (\ x -> isAlphaNum x || x `elem` ['\\', '.', '-'])
  return $ T.append a b

-- Instructions
iseq :: Parser M.ISeq
iseq = M.ISeq . Seq.fromList <$> (braces $ sepEndBy instruction sem)

instruction :: Parser M.I
instruction = (symbol "DROP" >> return M.DROP)
    <|> (do symbol "DUP"; v <- nv; return $ M.DUP v)
    <|> (do symbol "SWAP"; return M.SWAP)
    <|> (do symbol "PUSH"; v <- nv; a <- m_type; b <- m_data;
            return $ M.PUSH v a b)
    <|> (do symbol "SOME"; t <- nt; v <- nv; f <- nf; return $ M.SOME t v f)
    <|> (do symbol "NONE"; t <- nt; v <- nv; f <- nf; a <- m_type;
            return $ M.NONE t v f a)
    <|> (do symbol "UNIT"; t <- nt; return $ M.UNIT t)
    <|> (do symbol "IF_NONE"; a <- iseq; b <- iseq; return $ M.IF_NONE a b)
    <|> (do symbol "PAIR"; t <- nt; v <- nv; f <- nf; f' <- nf;
            return $ M.PAIR t v f f')
    <|> (do symbol "CAR"; v <- nv; f <- nf; return $ M.CAR v f)
    <|> (do symbol "CDR"; v <- nv; f <- nf; return $ M.CDR v f)
    <|> (do symbol "LEFT"; t <- nt; v <- nv; f <- nf; f' <- nf; a <- m_type;
            return $ M.LEFT t v f f' a)
    <|> (do symbol "RIGHT"; t <- nt; v <- nv; f <- nf; f' <- nf; a <- m_type;
            return $ M.RIGHT t v f f' a)
    <|> (do symbol "NIL"; t <- nt; v <- nv; a <- m_type; return $ M.NIL t v a)
    <|> (do symbol "CONS"; v <- nv; return $ M.CONS v)
    <|> (do symbol "IF_CONS"; a <- iseq; b <- iseq; return $ M.IF_CONS a b)
    <|> (do symbol "EMPTY_SET"; t <- nt; v <- nv; a <- m_ct;
            return $ M.EMPTY_SET t v a)
    <|> (do symbol "EMPTY_MAP"; t <- nt; v <- nv; a <- m_ct; b <- m_type;
            return $ M.EMPTY_SET t v a)
    <|> (do symbol "MAP"; v <- nv; a <- iseq; return $ M.MAP v a)
    <|> (do symbol "ITER"; v <- nv; a <- iseq; return $ M.ITER v a)
    <|> (do symbol "MEM"; v <- nv; return $ M.MEM v)
    <|> (do symbol "GET"; v <- nv; return $ M.GET v)
    <|> (do symbol "UPDATE"; return M.UPDATE)
    <|> (do symbol "IF"; a <- iseq; b <- iseq; return $ M.IF a b)
    <|> (do symbol "LOOP"; a <- iseq; return $ M.LOOP a)
    <|> (do symbol "LOOP_LEFT"; a <- iseq; return $ M.LOOP_LEFT a)
    <|> (do symbol "LAMBDA"; v <- nv; a <- m_type; b <- m_type; c <- iseq;
            return $ M.LAMBDA v a b c)
    <|> (do symbol "EXEC"; v <- nv; return $ M.EXEC v)
    <|> (do symbol "DIP"; a <- iseq; return $ M.DIP a)
    <|> (do symbol "FAILWITH"; a <- m_data; return $ M.FAILWITH a)
    <|> (do symbol "CAST"; t <- nt; v <- nv; return $ M.CAST t v)
    <|> (do symbol "RENAME"; v <- nv; return $ M.RENAME v)
    <|> (do symbol "CONCAT"; v <- nv; return $ M.CONCAT v)
    <|> (do symbol "ADD"; v <- nv; return $ M.ADD v)
    <|> (do symbol "SUB"; v <- nv; return $ M.SUB v)
    <|> (do symbol "MUL"; v <- nv; return $ M.MUL v)
    <|> (do symbol "EDIV"; v <- nv; return $ M.EDIV v)
    <|> (do symbol "ABS"; v <- nv; return $ M.ABS v)
    <|> (do symbol "NEG"; v <- nv; return M.NEG )
    <|> (do symbol "MOD"; v <- nv; return M.MOD )
    <|> (do symbol "LSL"; v <- nv; return $ M.LSL v)
    <|> (do symbol "LSR"; v <- nv; return $ M.LSR v)
    <|> (do symbol "OR";  v <- nv; return $ M.OR v)
    <|> (do symbol "AND"; v <- nv; return $ M.AND v)
    <|> (do symbol "NOT"; v <- nv; return $ M.NOT v)
    <|> (do symbol "COMPARE"; v <- nv;return $ M.COMPARE v)
    <|> (do symbol "EQ"; v <- nv; return $ M.EQ v)
    <|> (do symbol "NEQ"; v <- nv; return $ M.NEQ v)
    <|> (do symbol "LT"; v <- nv; return $ M.LT v)
    <|> (do symbol "GT"; v <- nv; return $ M.GT v)
    <|> (do symbol "LE"; v <- nv; return $ M.LE v)
    <|> (do symbol "GE"; v <- nv; return $ M.GE v)
    <|> (do symbol "INT"; v <- nv;return $ M.INT v)
    <|> (do symbol "SELF"; v <- nv;return $ M.SELF v)
    <|> (do symbol "TRANSFER_TOKENS"; return $ M.TRANSFER_TOKENS)
    <|> (do symbol "SET_DELEGATE"; return $ M.SET_DELEGATE)
    <|> (do symbol "CREATE_ACCOUNT"; v <- nv; v' <- nv;
            return $ M.CREATE_ACCOUNT v v')
    <|> (do symbol "CREATE_CONTRACT"; v <- nv; v' <- nv; a <- iseq;
            return $ M.CREATE_CONTRACT2 v v' a)
    <|> (do symbol "CREATE_CONTRACT"; v <- nv; v' <- nv;
            return $ M.CREATE_CONTRACT v v')
    <|> (do symbol "IMPLICIT_ACCOUNT"; v <- nv; return $ M.IMPLICIT_ACCOUNT v)
    <|> (do symbol "NOW"; v <- nv; return $ M.NOW v)
    <|> (do symbol "AMOUNT"; v <- nv; return $ M.AMOUNT v)
    <|> (do symbol "BALANCE"; v <- nv; return $ M.BALANCE v)
    <|> (do symbol "CHECK_SIGNATURE"; v <- nv; return $ M.CHECK_SIGNATURE v)
    <|> (do symbol "BLAKE2B"; v <- nv; return $ M.BLAKE2B v)
    <|> (do symbol "HASH_KEY"; v <- nv; return $ M.HASH_KEY v)
    <|> (do symbol "STEPS_TO_QUOTA"; v <- nv; return $ M.STEPS_TO_QUOTA v)
    <|> (do symbol "SOURCE"; v <- nv; return $ M.SOURCE v)
    <|> (do symbol "SENDER"; v <- nv; return $ M.SENDER v)
    <|> (do symbol "ADDRESS"; v <- nv; return $ M.ADDRESS v)

-- Types

m_type :: Parser M.T
m_type = lexeme $ (m_type' <|> parens m_type')

m_type' :: Parser M.T
m_type' = (do ct <- m_ct; t <- nt; return $ M.T_comparable t ct)
  <|> (do symbol "key"; t <- nt; return $ M.T_key t)
  <|> (do symbol "unit"; t <- nt; return $ M.T_unit t)
  <|> (do symbol "signature"; t <- nt; return $ M.T_signature t)
  <|> (do symbol "option"; t <- nt; f <- nf; a <- m_type; return $ M.T_option t f a)
  <|> (do symbol "list"; t <- nt; a <- m_type; return $ M.T_list t a)
  <|> (do symbol "set"; t <- nt; a <- m_ct; return $ M.T_set t a)
  <|> (do symbol "operation"; t <- nt; return $ M.T_operation t)
  <|> (do symbol "contract"; t <- nt; a <- m_type; return $ M.T_contract t a)
  <|> (do symbol "pair"; t <- nt; f <- nf; f' <- nf; a <- m_type; b <- m_type;
          return $ M.T_pair t f f' a b)
  <|> (do symbol "or"; t <- nt; f <- nf; f' <- nf; a <- m_type; b <- m_type;
          return $ M.T_or t f f' a b)
  <|> (do symbol "lambda"; t <- nt; a <- m_type; b <- m_type;
          return $ M.T_lambda t a b)
  <|> (do symbol "map"; t <- nt; a <- m_ct; b <- m_type;
          return $ M.T_map t a b)
  <|> (do symbol "big_map"; t <- nt; a <- m_ct; b <- m_type;
          return $ M.T_big_map t a b)

-- Comparable Types
m_ct :: Parser M.CT
m_ct = (symbol "int" >> return M.T_int)
 <|> (symbol "nat" >> return M.T_nat)
 <|> (symbol "string" >> return M.T_string)
 <|> (symbol "mutez" >> return M.T_mutez)
 <|> (symbol "bool" >> return M.T_bool)
 <|> (symbol "key_hash" >> return M.T_key_hash)
 <|> (symbol "timestamp" >> return M.T_timestamp)
