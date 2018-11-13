{-# LANGUAGE OverloadedStrings #-}
module Language.Michelson.Parser where

import           Control.Applicative.Permutations

import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as B16
import           Data.Char                        as Char
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Text.IO                     as TIO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

import qualified Language.Michelson.Types         as M

import           Data.Maybe
import           Data.Natural
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq
import           Data.Void                        (Void)

import           Control.Applicative.Permutations

parseFile :: String -> IO M.Contract
parseFile file = do
  code <- TIO.readFile file
  case parse contract file code of
    Left e   -> print e >> fail "error"
    Right sc -> return sc

--emptyContract = M.Contract (M.Parameter M.T_unit)
--               (M.Storage M.T_unit)
--               (M.Code (M.ISeq Seq.Empty))

contract :: Parser M.Contract
contract = do
  mSpace
  (p,s,c) <- runPermutation $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ M.Contract p s c


parameter :: Parser M.Parameter
parameter = do
  symbol "parameter"
  a <- type_
  semicolon
  return $ M.Parameter a

storage :: Parser M.Storage
storage = do
  symbol "storage"
  a <- type_
  semicolon
  return $ M.Storage a

code :: Parser M.Code
code = do
  symbol "code";
  a <- ops
  optional semicolon
  return $ M.Code a


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
          <|> (M.DataOps <$> ops)

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

-- Ops
ops :: Parser M.Ops
ops = (M.|:) <$> (braces $ sepEndBy (op <|> (M.OpsSeq <$> macro)) semicolon)



op :: Parser M.Op
op = drop_
  <|> dup_
  <|> swap_
  <|> push_
  <|> some_
  <|> none_
  <|> unit_
  <|> ifNone_
  <|> pair_
  <|> car_
  <|> cdr_
  <|> left_
  <|> right_
  <|> nil_
  <|> cons_
  <|> ifCons_
  <|> emptySet_
  <|> emptyMap_
  <|> map_
  <|> iter_
  <|> mem_
  <|> get_
  <|> update_
  <|> if_
  <|> loop_
  <|> loopLeft_
  <|> lambda_
  <|> exec_
  <|> dip_
  <|> failWith_
  <|> cast_
  <|> rename_
  <|> concat_
  <|> add_
  <|> sub_
  <|> ediv_
  <|> abs_
  <|> neg_
  <|> mod_
  <|> lsl_
  <|> lsr_
  <|> or_
  <|> and_
  <|> not_
  <|> compare_
  <|> eq_
  <|> neq_
  <|> lt_
  <|> gt_
  <|> ge_
  <|> int_
  <|> self_
  <|> transferTokens_
  <|> setDelegate_
  <|> createAccount_
  <|> createContract2_
  <|> createContract_
  <|> implicitAccount_
  <|> now_
  <|> amount_
  <|> balance_
  <|> checkSignature_
  <|> blake2B_
  <|> hashKey_
  <|> stepsToQuota_
  <|> source_
  <|> sender_
  <|> address_

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

drop_ = symbol "DROP" >> return M.DROP

dup_ = do
  symbol "DUP"
  v <- varNote
  return $ M.DUP v

swap_ = symbol "SWAP" >> return M.SWAP

push_ = do
  symbol "PUSH"
  v <- varNote
  a <- type_
  b <- data_
  return $ M.PUSH v a b

some_ = do
  symbol "SOME"
  (t, v, f) <- permute3 typeNote varNote fieldNote
  return $ M.SOME t v f

none_ = do
  symbol "NONE"
  (t, v, f) <- permute3 typeNote varNote fieldNote
  a <- type_
  return $ M.NONE t v f a

unit_ = do
  symbol "UNIT"
  t <- typeNote
  return $ M.UNIT t

ifNone_ = do
  symbol "IF_NONE"
  a <- ops
  b <- ops;
  return $ M.IF_NONE a b

pair_ = do
  symbol "PAIR"
  (t, v, (f, f')) <- permute3 typeNote varNote fields2
  return $ M.PAIR t v f f'

car_ = do
  symbol "CAR"
  v <- varNote; f <- fieldNote
  return $ M.CAR v f

cdr_ = do
  symbol "CDR"
  v <- varNote
  f <- fieldNote
  return $ M.CDR v f

left_ = do
  symbol "LEFT"
  (t, v, (f, f')) <- permute3 typeNote varNote fields2
  a <- type_
  return $ M.LEFT t v f f' a

right_ = do
  symbol "RIGHT"
  (t, v, (f, f')) <- permute3 typeNote varNote fields2
  a <- type_
  return $ M.RIGHT t v f f' a

nil_ = do
  symbol "NIL"
  (t, v) <- permute2 typeNote varNote
  a <- type_;
  return $ M.NIL t v a

cons_ = do
  symbol "CONS"
  v <- varNote
  return $ M.CONS v

ifCons_ = do
  symbol "IF_CONS"
  a <- ops
  b <- ops
  return $ M.IF_CONS a b

emptySet_ = do
  symbol "EMPTY_SET"
  t <- typeNote
  v <- varNote
  a <- comparableType
  return $ M.EMPTY_SET t v a

emptyMap_ = do
  symbol "EMPTY_MAP"
  t <- typeNote
  v <- varNote
  a <- comparableType
  b <- type_
  return $ M.EMPTY_MAP t v a b

map_ = do
  symbol "MAP"
  v <- varNote
  a <- ops
  return $ M.MAP v a

iter_ = do
  symbol "ITER"
  v <- varNote
  a <- ops
  return $ M.ITER v a

mem_ = do
  symbol "MEM"
  v <- varNote
  return $ M.MEM v

get_ = do
  symbol "GET"
  v <- varNote
  return $ M.GET v

update_ = symbol "UPDATE" >> return M.UPDATE

if_ = do
  symbol "IF"
  a <- ops
  b <- ops
  return $ M.IF a b

loop_ = do
  symbol "LOOP"
  a <- ops
  return $ M.LOOP a

loopLeft_ = do
  symbol "LOOP_LEFT"
  a <- ops
  return $ M.LOOP_LEFT a

lambda_ = do
  symbol "LAMBDA"
  v <- varNote
  a <- type_
  b <- type_
  c <- ops
  return $ M.LAMBDA v a b c

exec_ = do
  symbol "EXEC"
  v <- varNote
  return $ M.EXEC v

dip_ = do
  symbol "DIP"
  a <- ops
  return $ M.DIP a

failWith_ = symbol "FAILWITH" >> return M.FAILWITH

cast_ = do
  symbol "CAST"
  t <- typeNote
  v <- varNote
  return $ M.CAST t v

rename_ = do
  symbol "RENAME"
  v <- varNote
  return $ M.RENAME v

concat_ = do
  symbol "CONCAT"
  v <- varNote
  return $ M.CONCAT v

add_ = do
  symbol "ADD"
  v <- varNote
  return $ M.ADD v

sub_ = do
  symbol "SUB"
  v <- varNote
  return $ M.SUB v

mul_ = do
  symbol "MUL"
  v <- varNote
  return $ M.MUL v

ediv_ = do
  symbol "EDIV"
  v <- varNote
  return $ M.EDIV v

abs_ = do
  symbol "ABS"
  v <- varNote
  return $ M.ABS v

neg_ = do
  symbol "NEG"
  -- v <- varNote
  return M.NEG

mod_ = do
  symbol "MOD"
  -- v <- varNote
  return M.MOD

lsl_ = do
  symbol "LSL"
  v <- varNote
  return $ M.LSL v

lsr_ = do
  symbol "LSR"
  v <- varNote
  return $ M.LSR v

or_ = do
  symbol "OR"
  v <- varNote
  return $ M.OR v

and_ = do
  symbol "AND"
  v <- varNote
  return $ M.AND v

not_ = do
  symbol "NOT"
  v <- varNote
  return $ M.NOT v

compare_ = do
  symbol "COMPARE"
  v <- varNote
  return $ M.COMPARE v

eq_ = do
  symbol "EQ"
  v <- varNote
  return $ M.EQ v

neq_ = do
  symbol "NEQ"
  v <- varNote
  return $ M.NEQ v

lt_ = do
  symbol "LT"
  v <- varNote
  return $ M.LT v

gt_ = do
  symbol "GT"
  v <- varNote
  return $ M.GT v

le_ = do
  symbol "LE"
  v <- varNote
  return $ M.LE v

ge_ = do
  symbol "GE"
  v <- varNote
  return $ M.GE v

int_ = do
  symbol "INT"
  v <- varNote
  return $ M.INT v

self_ = do
  symbol "SELF"
  v <- varNote
  return $ M.SELF v

transferTokens_ = do
  symbol "TRANSFER_TOKENS"
  return $ M.TRANSFER_TOKENS

setDelegate_ = do
  symbol "SET_DELEGATE"
  return $ M.SET_DELEGATE

createAccount_ = do
  symbol "CREATE_ACCOUNT"
  v <- varNote
  v' <- varNote
  return $ M.CREATE_ACCOUNT v v'

createContract2_ = do
  symbol "CREATE_CONTRACT"
  v <- varNote
  v' <- varNote
  a <- ops
  return $ M.CREATE_CONTRACT2 v v' a

createContract_ = do
  symbol "CREATE_CONTRACT"
  v <- varNote
  v' <- varNote
  return $ M.CREATE_CONTRACT v v'

implicitAccount_ = do
  symbol "IMPLICIT_ACCOUNT"
  v <- varNote
  return $ M.IMPLICIT_ACCOUNT v

now_ = do
  symbol "NOW"
  v <- varNote
  return $ M.NOW v

amount_ = do
  symbol "AMOUNT"
  v <- varNote
  return $ M.AMOUNT v

balance_ = do
  symbol "BALANCE"
  v <- varNote
  return $ M.BALANCE v

checkSignature_ = do
  symbol "CHECK_SIGNATURE"
  v <- varNote
  return $ M.CHECK_SIGNATURE v

blake2B_ = do
  symbol "BLAKE2B"
  v <- varNote
  return $ M.BLAKE2B v

hashKey_ = do
  symbol "HASH_KEY"
  v <- varNote
  return $ M.HASH_KEY v

stepsToQuota_ = do
  symbol "STEPS_TO_QUOTA"
  v <- varNote
  return $ M.STEPS_TO_QUOTA v

source_ = do
  symbol "SOURCE"
  v <- varNote
  return $ M.SOURCE v

sender_ = do
  symbol "SENDER"
  v <- varNote
  return $ M.SENDER v

address_ = do
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
           <|> T.singleton <$> satisfy isAlphaNum
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
typeInner = (do ct <- ct; t <- tN; return $ M.T_comparable t ct)
  <|> (do symbol "key"; t <- tN; return $ M.T_key t)
  <|> (do symbol "unit"; t <- tN; return $ M.T_unit t)
  <|> (do symbol "signature"; t <- tN; return $ M.T_signature t)
  <|> (do symbol "option"; t <- tN; (a, f) <- tF; return $ M.T_option t f a)
  <|> (do symbol "list"; t <- tN; a <- type_; return $ M.T_list t a)
  <|> (do symbol "set"; t <- tN; a <- ct; return $ M.T_set t a)
  <|> (do symbol "operation"; t <- tN; return $ M.T_operation t)
  <|> (do symbol "contract"; t <- tN; a <- type_; return $ M.T_contract t a)
  <|> (do symbol "pair"; t <- tN; (a, f) <- tF; (b, f') <- tF;
          return $ M.T_pair t f f' a b)
  <|> (do symbol "or"; t <- tN; (a, f) <- tF; (b, f') <- tF;
          return $ M.T_or t f f' a b)
  <|> (do symbol "lambda"; t <- tN; a <- type_; b <- type_;
          return $ M.T_lambda t a b)
  <|> (do symbol "map"; t <- tN; a <- ct; b <- type_;
          return $ M.T_map t a b)
  <|> (do symbol "big_map"; t <- tN; a <- ct; b <- type_;
          return $ M.T_big_map t a b)
  where
    ct = comparableType
    tN = typeNote
    tF = typeField

-- Comparable Types
comparableType :: Parser M.ComparableType
comparableType = (symbol "int" >> return M.T_int)
 <|> (symbol "nat" >> return M.T_nat)
 <|> (symbol "string" >> return M.T_string)
 <|> (symbol "mutez" >> return M.T_mutez)
 <|> (symbol "bool" >> return M.T_bool)
 <|> (symbol "key_hash" >> return M.T_key_hash)
 <|> (symbol "timestamp" >> return M.T_timestamp)

-- Macros

macro :: Parser M.Ops
macro = cmpMac
  <|> ifMac
  <|> ifcmpMac
  <|> failMac
  <|> assert
  <|> assertMac
  <|> assertCmpMac
  <|> assertNoneMac
  <|> assertSomeMac
  <|> assertLeftMac
  <|> assertRightMac
  <|> dipMac
  <|> dupMac
  <|> pairMac
  <|> unpairMac

cmpOp = eq_ <|> neq_ <|> lt_ <|> gt_ <|> le_ <|> gt_


cmpMac :: Parser M.Ops
cmpMac = do
  string "CMP"
  a <- cmpOp
  return $ cmpX a

cmpX :: M.Op -> M.Ops
cmpX op = M.opsFromList [M.COMPARE noVNote, op]

ifMac :: Parser M.Ops
ifMac = do
  string "IF"
  a <- cmpOp
  bt <- ops
  bf <- ops
  return $ ifX a bt bf

ifX :: M.Op -> M.Ops -> M.Ops -> M.Ops
ifX op bt bf = M.opsFromList [op, M.IF bt bf]

ifcmpMac :: Parser M.Ops
ifcmpMac = do
  string "IFCMP"
  a <- cmpOp
  bt <- ops
  bf <- ops
  return $ ifcmpX a bt bf

ifcmpX :: M.Op -> M.Ops -> M.Ops -> M.Ops
ifcmpX op bt bf = M.opsFromList [M.COMPARE noVNote, op, M.IF bt bf]

-- fail

failMac :: Parser M.Ops
failMac = symbol "FAIL" >> return failX

failX :: M.Ops
failX = M.opsFromList [M.UNIT noTNote, M.FAILWITH]

{- assertion macros:
  * assert
  * assert_{eq}
  * assert_none
  * assert_some
  * assert_left
  * assert_right
-}


assert :: Parser M.Ops
assert = do
  symbol "ASSERT"
  return $ M.opsFromList [M.IF M.noOps failX]

assertMac :: Parser M.Ops
assertMac = do
  string "ASSERT_"
  a <- cmpOp
  return $ ifX a (M.noOps) failX

assertCmpMac :: Parser M.Ops
assertCmpMac = do
  string "ASSERT_CMP"
  a <- cmpOp
  return $ ifcmpX a (M.noOps) failX

assertNoneMac :: Parser M.Ops
assertNoneMac = do
  symbol "ASSERT_NONE"
  return $ M.opsFromList [M.IF_NONE M.noOps failX]

assertSomeMac :: Parser M.Ops
assertSomeMac = do
  symbol "ASSERT_SOME"
  return $ M.opsFromList [M.IF_NONE failX M.noOps]

assertLeftMac :: Parser M.Ops
assertLeftMac = do
  symbol "ASSERT_LEFT"
  return $ M.opsFromList [M.IF_LEFT M.noOps failX]

assertRightMac :: Parser M.Ops
assertRightMac = do
  symbol "ASSERT_RIGHT"
  return $ M.opsFromList [M.IF_LEFT failX M.noOps]

{- syntactic "conveniences"
   * DII+P code
   * DUU+P
   * [PAIR]
   * UNPAIR
   * C[AD]R
   * IF_SOME
   * SET_C[AD]R
   * MAP_C[AD]R
-}

dipMac :: Parser M.Ops
dipMac = do
  string "DI"
  n <- fromIntegral . (+1) . length <$> (some $ string "I")
  symbol "P"
  c <- ops
  return $ dipX n c

dipX :: Natural -> M.Ops -> M.Ops
dipX n code = go n (M.opsFromList [M.DIP code])
  where
    go 1 x = x
    go n x = go (n - 1) (M.opsFromList $ [M.DIP x])

dupMac :: Parser M.Ops
dupMac = do
  string "DU"
  n <- fromIntegral .(+1) . length <$> (some $ string "U")
  symbol "P"
  return $ dupX n

-- TODO: annotation?
dupX :: Natural -> M.Ops
dupX n = go n (M.opsFromList [M.DIP $ M.opsFromList [(M.DUP noVNote)], M.SWAP])
  where
    go 1 x = x
    go n x = go (n - 1) (M.opsFromList [M.DIP x, M.SWAP])

--pair
data P = Stop | Nest P P deriving Show

pairMac :: Parser M.Ops
pairMac = pairX <$> (pairMacInner <* symbol "R")

pairMacInner :: Parser P
pairMacInner = do
  string "P"
  l <- (string "A" >> return Stop) <|> pairMacInner
  r <- (string "I" >> return Stop) <|> pairMacInner
  return $ Nest l r

pairX :: P -> M.Ops
pairX (Nest Stop Stop) = M.opsFromList $ [pairN]
pairX (Nest Stop r) = M.opsFromList $ [M.DIP $ pairX r, pairN]
pairX (Nest l Stop) = M.opsConcat (M.opsFromList [pairN]) (pairX l)
pairX (Nest l r) =
  M.opsConcat (M.opsConcat (pairX l) (pairX r)) (M.opsFromList [pairN])

pairN = (M.PAIR noTNote noVNote noFNote noFNote)

--unpair
unpairMac :: Parser M.Ops
unpairMac = unpairX <$> (string "UN" >> pairMacInner <* symbol "R")

unpairX :: P -> M.Ops
unpairX (Nest Stop Stop) =
  (M.|:) [M.DUP noVNote, M.CAR noVNote noFNote, M.DIP $ (M.|:) [M.CDR noVNote noFNote]]
unpairX (Nest Stop r) = unpairN M.>< (M.|:) [M.DIP $ unpairX r]
unpairX (Nest l Stop) = unpairN M.>< (unpairX l)
unpairX (Nest l r) = (unpairN) M.>< (unpairX l) M.>< (unpairX r)

unpairN = unpairX (Nest Stop Stop)

--cadr
--
--if_some
--
--set_cadr
--
--map_cadr
--
--
