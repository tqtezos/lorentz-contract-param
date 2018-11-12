{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Michelson.Parser.Prim where

import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as B16
import           Data.Char                        as Char
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Text.IO                     as TIO
import qualified Text.Megaparsec                  as P
import  Text.Megaparsec hiding (some)
import           Text.Megaparsec.Char             as C
import qualified Text.Megaparsec.Char.Lexer       as L

import qualified Language.Michelson.Types         as M

import           Data.Natural
import           Data.Maybe
import qualified Data.Sequence                    as Seq
import Data.Sequence (Seq)
import           Data.Void                        (Void)

import           Control.Applicative.Permutations

import Prelude hiding (not, some, and, compare, concat, abs, mod, or, drop, map)

type Parser = Parsec Void T.Text

-- Lexing
lexeme = L.lexeme mSpace
mSpace = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol = L.symbol mSpace
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
semicolon = symbol ";"

-- Data
data_ :: Parser M.Data
data_ = lexeme $ (dataInner <|> parens dataInner)
  where
    dataInner = (try $ M.Int <$> literalInt)
          <|> (try $ M.String <$> literalString)
          <|> (try $ M.Bytes <$> literalBytes)
          <|> (do symbol "Unit"; return M.Unit)
          <|> (do symbol "True"; return M.True)
          <|> (do symbol "False"; return M.False)
          <|> (do symbol "Pair"; a <- data_; b <- data_; return $ M.Pair a b)
          <|> (do symbol "Left"; a <- data_; return $ M.Left a)
          <|> (do symbol "Right"; a <- data_; return $ M.Right a)
          <|> (do symbol "Some"; a <- data_; return $ M.Some a)
          <|> (do symbol "None"; return M.None)
          <|> (try $ M.Seq <$> dataList)
          <|> (try $ M.Map <$> dataMap)
          <|> (M.DataOps <$> instructions)

literalInt :: Parser Integer
literalInt = (L.signed (return ()) L.decimal)

literalBytes :: Parser B.ByteString
literalBytes = do
  symbol "0x"
  hexdigits <- takeWhile1P Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return bytes
  else (error "odd number bytes") -- TODO: better errors

-- this parses more escape sequences than are in the michelson spec
-- should investigate which sequences this matters for, e.g. \xa == \n
literalString :: Parser T.Text
literalString = T.pack <$> (char '"' >> (manyTill L.charLiteral (char '"')))

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


dataList :: Parser (Seq M.Data)
dataList = Seq.fromList <$> (braces $ sepEndBy data_ semicolon)

dataElement :: Parser M.Element
dataElement = do
  symbol "Elt"
  key <- data_
  val <- data_
  return $ M.Element key val

dataMap :: Parser (Seq M.Element)
dataMap = Seq.fromList <$> (braces $ sepEndBy dataElement semicolon)

-- Instructions
instructions :: Parser M.Instructions
instructions =
  M.Instructions . Seq.fromList <$> (braces $ sepEndBy op semicolon)

op :: Parser M.Op
op = drop
  <|> dup
  <|> swap
  <|> push
  <|> some
  <|> none
  <|> unit
  <|> ifNone
  <|> pair
  <|> car
  <|> cdr
  <|> left
  <|> right
  <|> nil
  <|> cons
  <|> ifCons
  <|> emptySet
  <|> emptyMap
  <|> map
  <|> iter
  <|> mem
  <|> get
  <|> update
  <|> if_
  <|> loop
  <|> loopLeft
  <|> lambda
  <|> exec
  <|> dip
  <|> failWith
  <|> cast
  <|> rename
  <|> concat
  <|> add
  <|> sub
  <|> ediv
  <|> abs
  <|> neg
  <|> mod
  <|> lsl
  <|> lsr
  <|> or
  <|> and
  <|> not
  <|> compare
  <|> eq
  <|> neq
  <|> lt
  <|> gt
  <|> ge
  <|> int
  <|> self
  <|> transferTokens
  <|> setDelegate
  <|> createAccount
  <|> createContract2
  <|> createContract
  <|> implicitAccount
  <|> now
  <|> amount
  <|> balance
  <|> checkSignature
  <|> blake2B
  <|> hashKey
  <|> stepsToQuota
  <|> source
  <|> sender
  <|> address

permute2 :: Parser a -> Parser b -> Parser (a,b)
permute2 x y = runPermutation $
  (,) <$> toPermutation x
      <*> toPermutation y

permute3 :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
permute3 x y z = runPermutation $
  (,,) <$> toPermutation x
       <*> toPermutation y
       <*> toPermutation z

fields2 :: Parser (M.FieldAnnotation, M.FieldAnnotation)
fields2 = do
  a <- fieldNote
  b <- fieldNote
  return (a, b)

drop = symbol "DROP" >> return M.DROP

dup = do
  symbol "DUP"
  v <- varNote
  return $ M.DUP v

swap = symbol "SWAP" >> return M.SWAP

push = do
  symbol "PUSH"
  v <- varNote
  a <- type_
  b <- data_
  return $ M.PUSH v a b

some = do
  symbol "SOME"
  (t, v, f) <- permute3 typeNote varNote fieldNote
  return $ M.SOME t v f

none = do
  symbol "NONE"
  (t, v, f) <- permute3 typeNote varNote fieldNote
  a <- type_
  return $ M.NONE t v f a

unit = do
  symbol "UNIT"
  t <- typeNote
  return $ M.UNIT t

ifNone = do
  symbol "IF_NONE"
  a <- instructions
  b <- instructions;
  return $ M.IF_NONE a b

pair = do
  symbol "PAIR"
  (t, v, (f, f')) <- permute3 typeNote varNote fields2
  return $ M.PAIR t v f f'

car = do
  symbol "CAR"
  v <- varNote; f <- fieldNote
  return $ M.CAR v f

cdr = do
  symbol "CDR"
  v <- varNote
  f <- fieldNote
  return $ M.CDR v f

left = do
  symbol "LEFT"
  (t, v, (f, f')) <- permute3 typeNote varNote fields2
  a <- type_
  return $ M.LEFT t v f f' a

right = do
  symbol "RIGHT"
  (t, v, (f, f')) <- permute3 typeNote varNote fields2
  a <- type_
  return $ M.RIGHT t v f f' a

nil = do
  symbol "NIL"
  (t, v) <- permute2 typeNote varNote
  a <- type_;
  return $ M.NIL t v a

cons = do
  symbol "CONS"
  v <- varNote
  return $ M.CONS v

ifCons = do
  symbol "IF_CONS"
  a <- instructions
  b <- instructions
  return $ M.IF_CONS a b

emptySet = do
  symbol "EMPTY_SET"
  t <- typeNote
  v <- varNote
  a <- comparableType
  return $ M.EMPTY_SET t v a

emptyMap = do
  symbol "EMPTY_MAP"
  t <- typeNote
  v <- varNote
  a <- comparableType
  b <- type_
  return $ M.EMPTY_MAP t v a b

map = do
  symbol "MAP"
  v <- varNote
  a <- instructions
  return $ M.MAP v a

iter = do
  symbol "ITER"
  v <- varNote
  a <- instructions
  return $ M.ITER v a

mem = do
  symbol "MEM"
  v <- varNote
  return $ M.MEM v

get = do
  symbol "GET"
  v <- varNote
  return $ M.GET v

update = symbol "UPDATE" >> return M.UPDATE

if_ = do
  symbol "IF"
  a <- instructions
  b <- instructions
  return $ M.IF a b

loop = do
  symbol "LOOP"
  a <- instructions
  return $ M.LOOP a

loopLeft = do
  symbol "LOOP_LEFT"
  a <- instructions
  return $ M.LOOP_LEFT a

lambda = do
  symbol "LAMBDA"
  v <- varNote
  a <- type_
  b <- type_
  c <- instructions
  return $ M.LAMBDA v a b c

exec = do
  symbol "EXEC"
  v <- varNote
  return $ M.EXEC v

dip = do
  symbol "DIP"
  a <- instructions
  return $ M.DIP a

failWith = do
  symbol "FAILWITH"
  a <- data_
  return $ M.FAILWITH a

cast = do
  symbol "CAST"
  t <- typeNote
  v <- varNote
  return $ M.CAST t v

rename = do
  symbol "RENAME"
  v <- varNote
  return $ M.RENAME v

concat = do
  symbol "CONCAT"
  v <- varNote
  return $ M.CONCAT v

add = do
  symbol "ADD"
  v <- varNote
  return $ M.ADD v

sub = do
  symbol "SUB"
  v <- varNote
  return $ M.SUB v

mul = do
  symbol "MUL"
  v <- varNote
  return $ M.MUL v

ediv = do
  symbol "EDIV"
  v <- varNote
  return $ M.EDIV v

abs = do
  symbol "ABS"
  v <- varNote
  return $ M.ABS v

neg = do
  symbol "NEG"
  -- v <- varNote
  return M.NEG

mod = do
  symbol "MOD"
  -- v <- varNote
  return M.MOD

lsl = do
  symbol "LSL"
  v <- varNote
  return $ M.LSL v

lsr = do
  symbol "LSR"
  v <- varNote
  return $ M.LSR v

or = do
  symbol "OR"
  v <- varNote
  return $ M.OR v

and = do
  symbol "AND"
  v <- varNote
  return $ M.AND v

not = do
  symbol "NOT"
  v <- varNote
  return $ M.NOT v

compare = do
  symbol "COMPARE"
  v <- varNote
  return $ M.COMPARE v

eq = do
  symbol "EQ"
  v <- varNote
  return $ M.EQ v
neq = do
  symbol "NEQ"
  v <- varNote
  return $ M.NEQ v
lt = do
  symbol "LT"
  v <- varNote
  return $ M.LT v
gt = do
  symbol "GT"
  v <- varNote
  return $ M.GT v
le = do
  symbol "LE"
  v <- varNote
  return $ M.LE v
ge = do
  symbol "GE"
  v <- varNote
  return $ M.GE v

int = do
  symbol "INT"
  v <- varNote
  return $ M.INT v

self = do
  symbol "SELF"
  v <- varNote
  return $ M.SELF v

transferTokens = do
  symbol "TRANSFER_TOKENS"
  return $ M.TRANSFER_TOKENS

setDelegate = do
  symbol "SET_DELEGATE"
  return $ M.SET_DELEGATE

createAccount = do
  symbol "CREATE_ACCOUNT"
  v <- varNote
  v' <- varNote
  return $ M.CREATE_ACCOUNT v v'

createContract2 = do
  symbol "CREATE_CONTRACT"
  v <- varNote
  v' <- varNote
  a <- instructions
  return $ M.CREATE_CONTRACT2 v v' a

createContract = do
  symbol "CREATE_CONTRACT"
  v <- varNote
  v' <- varNote
  return $ M.CREATE_CONTRACT v v'

implicitAccount = do
  symbol "IMPLICIT_ACCOUNT"
  v <- varNote
  return $ M.IMPLICIT_ACCOUNT v

now = do
  symbol "NOW"
  v <- varNote
  return $ M.NOW v

amount = do
  symbol "AMOUNT"
  v <- varNote
  return $ M.AMOUNT v
balance = do
  symbol "BALANCE"
  v <- varNote
  return $ M.BALANCE v

checkSignature = do
  symbol "CHECK_SIGNATURE"
  v <- varNote
  return $ M.CHECK_SIGNATURE v

blake2B = do
  symbol "BLAKE2B"
  v <- varNote
  return $ M.BLAKE2B v

hashKey = do
  symbol "HASH_KEY"
  v <- varNote
  return $ M.HASH_KEY v

stepsToQuota = do
  symbol "STEPS_TO_QUOTA"
  v <- varNote
  return $ M.STEPS_TO_QUOTA v

source = do
  symbol "SOURCE"
  v <- varNote
  return $ M.SOURCE v

sender = do
  symbol "SENDER"
  v <- varNote
  return $ M.SENDER v

address = do
  symbol "ADDRESS"
  v <- varNote
  return $ M.ADDRESS v

-- Annotations
noTNote = M.TypeAnnotation Nothing
noFNote = M.FieldAnnotation Nothing
noVNote = M.VarAnnotation Nothing

typeNote :: Parser M.TypeAnnotation
typeNote = M.TypeAnnotation <$> note ':'


fieldNote :: Parser M.FieldAnnotation
fieldNote = M.FieldAnnotation <$> note '%'

varNote :: Parser M.VarAnnotation
varNote = M.VarAnnotation <$> note '@'

note :: Char -> Parser (Maybe T.Text)
note c = optional $ lexeme $ char c >> (note' <|> pure "")
  where
    note' = do
      a <- string "@"
           <|> string "%"
           <|> string "%%"
           <|> T.singleton <$> P.satisfy isAlphaNum
      b <- takeWhileP Nothing (\ x -> isAlphaNum x || x `elem` ['\\', '.', '-'])
      return $ T.append a b

-- Types

type_ :: Parser M.Type
type_ = lexeme $ (typeInner <|> parens typeInner)

typeField :: Parser (M.Type, M.FieldAnnotation)
typeField = lexeme $ (mtf <|> parens mtf)
  where
    mtf = do
      mt <- typeInner
      nf <- fieldNote
      return (mt, nf)

typeInner :: Parser M.Type
typeInner = (do comparableType <- comparableType; t <- typeNote; return $ M.T_comparable t comparableType)
  <|> (do symbol "key"; t <- typeNote; return $ M.T_key t)
  <|> (do symbol "unit"; t <- typeNote; return $ M.T_unit t)
  <|> (do symbol "signature"; t <- typeNote; return $ M.T_signature t)
  <|> (do symbol "option"; t <- typeNote; (a, f) <- typeField;
          return $ M.T_option t f a)
  <|> (do symbol "list"; t <- typeNote; a <- type_; return $ M.T_list t a)
  <|> (do symbol "set"; t <- typeNote; a <- comparableType; return $ M.T_set t a)
  <|> (do symbol "operation"; t <- typeNote; return $ M.T_operation t)
  <|> (do symbol "contract"; t <- typeNote; a <- type_; return $ M.T_contract t a)
  <|> (do symbol "pair"; t <- typeNote; (a, f) <- typeField; (b, f') <- typeField;
          return $ M.T_pair t f f' a b)
  <|> (do symbol "or"; t <- typeNote; (a, f) <- typeField; (b, f') <- typeField;
          return $ M.T_or t f f' a b)
  <|> (do symbol "lambda"; t <- typeNote; a <- type_; b <- type_;
          return $ M.T_lambda t a b)
  <|> (do symbol "map"; t <- typeNote; a <- comparableType; b <- type_;
          return $ M.T_map t a b)
  <|> (do symbol "big_map"; t <- typeNote; a <- comparableType; b <- type_;
          return $ M.T_big_map t a b)

-- Comparable Types
comparableType :: Parser M.ComparableType
comparableType = (symbol "int" >> return M.T_int)
 <|> (symbol "nat" >> return M.T_nat)
 <|> (symbol "string" >> return M.T_string)
 <|> (symbol "mutez" >> return M.T_mutez)
 <|> (symbol "bool" >> return M.T_bool)
 <|> (symbol "key_hash" >> return M.T_key_hash)
 <|> (symbol "timestamp" >> return M.T_timestamp)
