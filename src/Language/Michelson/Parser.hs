{-# LANGUAGE OverloadedStrings #-}
module Language.Michelson.Parser where

import           Control.Applicative.Permutations

import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as B16
import           Data.Char                        as Char
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
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
    dataInner = (try $ M.Int <$> intLiteral)
          <|> (try $ M.String <$> stringLiteral)
          <|> (try $ M.Bytes <$> bytesLiteral)
          <|> (do symbol "Unit"; return M.Unit)
          <|> (do symbol "True"; return M.True)
          <|> (do symbol "False"; return M.False)
          <|> (do symbol "Pair"; a <- data_; b <- data_; return $ M.Pair a b)
          <|> (do symbol "Left"; a <- data_; return $ M.Left a)
          <|> (do symbol "Right"; a <- data_; return $ M.Right a)
          <|> (do symbol "Some"; a <- data_; return $ M.Some a)
          <|> (do symbol "None"; return M.None)
          <|> (try $ M.Seq <$> listData)
          <|> (try $ M.Map <$> mapData)
          <|> (M.DataOps <$> ops)

intLiteral :: Parser Integer
intLiteral = (L.signed (return ()) L.decimal)

bytesLiteral :: Parser B.ByteString
bytesLiteral = do
  symbol "0x"
  hexdigits <- takeWhile1P Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return bytes
  else (error "odd number bytes") -- TODO: better errors

-- this parses more escape sequences than are in the michelson spec
-- should investigate which sequences this matters for, e.g. \xa == \n
stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> (char '"' >> (manyTill L.charLiteral (char '"')))

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


listData :: Parser (Seq M.Data)
listData = Seq.fromList <$> (braces $ sepEndBy data_ semicolon)

elementData :: Parser M.Element
elementData = do
  symbol "Elt"
  key <- data_
  val <- data_
  return $ M.Element key val

mapData :: Parser (Seq M.Element)
mapData = Seq.fromList <$> (braces $ sepEndBy elementData semicolon)

-- Ops
ops :: Parser M.Ops
ops = (M.|:) <$> (braces $ sepEndBy ((try op) <|> (M.OpsSeq <$> macro)) semicolon)

op :: Parser M.Op
op = dropOp
  <|> dupOp
  <|> swapOp
  <|> pushOp
  <|> someOp
  <|> noneOp
  <|> unitOp
  <|> ifNoneOp
  <|> pairOp
  <|> carOp
  <|> cdrOp
  <|> leftOp
  <|> rightOp
  <|> nilOp
  <|> consOp
  <|> ifConsOp
  <|> emptySetOp
  <|> emptyMapOp
  <|> mapOp
  <|> iterOp
  <|> memOp
  <|> getOp
  <|> updateOp
  <|> ifOp
  <|> loopOp
  <|> loopLeftOp
  <|> lambdaOp
  <|> execOp
  <|> dipOp
  <|> failWithOp
  <|> castOp
  <|> renameOp
  <|> concatOp
  <|> addOp
  <|> subOp
  <|> edivOp
  <|> absOp
  <|> negOp
  <|> modOp
  <|> lslOp
  <|> lsrOp
  <|> orOp
  <|> andOp
  <|> notOp
  <|> compareOp
  <|> eqOp
  <|> neqOp
  <|> ltOp
  <|> gtOp
  <|> geOp
  <|> intOp
  <|> selfOp
  <|> transferTokensOp
  <|> setDelegateOp
  <|> createAccountOp
  <|> createContract2Op
  <|> createContractOp
  <|> implicitAccountOp
  <|> nowOp
  <|> amountOp
  <|> balanceOp
  <|> checkSignatureOp
  <|> blake2BOp
  <|> hashKeyOp
  <|> stepsToQuotaOp
  <|> sourceOp
  <|> senderOp
  <|> addressOp

permute2WithDefault :: Parser a -> Parser b -> a -> b -> Parser (a,b)
permute2WithDefault x y a b = runPermutation $
  (,) <$> (toPermutationWithDefault a x)
      <*> (toPermutationWithDefault b y)

permute3WithDefault :: Parser a -> Parser b -> Parser c
                        -> a -> b -> c -> Parser (a,b,c)
permute3WithDefault x y z a b c= runPermutation $
  (,,) <$> (toPermutationWithDefault a x)
       <*> (toPermutationWithDefault b y)
       <*> (toPermutationWithDefault c z)


fields2 :: Parser (M.FieldNote, M.FieldNote)
fields2 = do
  a <- fieldNote'
  b <- fieldNote'
  return (a, b)

typeVarField1 :: Parser (M.TypeNote, M.VarNote, M.FieldNote)
typeVarField1 =
  permute3WithDefault typeNote' varNote' fieldNote' M.noTN M.noVN M.noFN

typeVarField2 :: Parser (M.TypeNote, M.VarNote, (M.FieldNote, M.FieldNote))
typeVarField2 =
  permute3WithDefault typeNote' varNote' fields2 M.noTN M.noVN (M.noFN, M.noFN)

typeAndVar :: Parser (M.TypeNote, M.VarNote)
typeAndVar = permute2WithDefault typeNote' varNote' M.noTN M.noVN

varAndField :: Parser (M.VarNote, M.FieldNote)
varAndField  = permute2WithDefault varNote' fieldNote' M.noVN M.noFN

dropOp = symbol "DROP" >> return M.DROP

dupOp = do
  symbol "DUP"
  v <- varNote
  return $ M.DUP v

swapOp = symbol "SWAP" >> return M.SWAP

pushOp = do
  symbol "PUSH"
  v <- varNote
  a <- type_
  b <- data_
  return $ M.PUSH v a b

someOp = do
  symbol "SOME"
  (t, v, f) <- typeVarField1
  return $ M.SOME t v f

noneOp = do
  symbol "NONE"
  (t, v, f) <- typeVarField1
  a <- type_
  return $ M.NONE t v f a

unitOp = do
  symbol "UNIT"
  t <- typeNote
  return $ M.UNIT t

ifNoneOp = do
  symbol "IF_NONE"
  a <- ops
  b <- ops;
  return $ M.IF_NONE a b

pairOp = do
  symbol "PAIR"
  (t, v, (f, f')) <- typeVarField2
  return $ M.PAIR t v f f'

carOp = do
  symbol "CAR"
  (v, f) <- varAndField
  return $ M.CAR v f

cdrOp = do
  symbol "CDR"
  (v, f) <- varAndField
  return $ M.CDR v f

leftOp = do
  symbol "LEFT"
  (t, v, (f, f')) <- typeVarField2
  a <- type_
  return $ M.LEFT t v f f' a

rightOp = do
  symbol "RIGHT"
  (t, v, (f, f')) <- typeVarField2
  a <- type_
  return $ M.RIGHT t v f f' a

nilOp = do
  symbol "NIL"
  (t, v) <- typeAndVar
  a <- type_;
  return $ M.NIL t v a

consOp = do
  symbol "CONS"
  v <- varNote
  return $ M.CONS v

ifConsOp = do
  symbol "IF_CONS"
  a <- ops
  b <- ops
  return $ M.IF_CONS a b

emptySetOp = do
  symbol "EMPTY_SET"
  (t, v) <- typeAndVar
  a <- comparable
  return $ M.EMPTY_SET t v a

emptyMapOp = do
  symbol "EMPTY_MAP"
  (t, v) <- typeAndVar
  a <- comparable
  a <- comparable
  b <- type_
  return $ M.EMPTY_MAP t v a b

mapOp = do
  symbol "MAP"
  v <- varNote
  a <- ops
  return $ M.MAP v a

iterOp = do
  symbol "ITER"
  v <- varNote
  a <- ops
  return $ M.ITER v a

memOp = do
  symbol "MEM"
  v <- varNote
  return $ M.MEM v

getOp = do
  symbol "GET"
  v <- varNote
  return $ M.GET v

updateOp = symbol "UPDATE" >> return M.UPDATE

ifOp = do
  symbol "IF"
  a <- ops
  b <- ops
  return $ M.IF a b

loopOp = do
  symbol "LOOP"
  a <- ops
  return $ M.LOOP a

loopLeftOp = do
  symbol "LOOP_LEFT"
  a <- ops
  return $ M.LOOP_LEFT a

lambdaOp = do
  symbol "LAMBDA"
  v <- varNote
  a <- type_
  b <- type_
  c <- ops
  return $ M.LAMBDA v a b c

execOp = do
  symbol "EXEC"
  v <- varNote
  return $ M.EXEC v

dipOp = do
  symbol "DIP"
  a <- ops
  return $ M.DIP a

failWithOp = symbol "FAILWITH" >> return M.FAILWITH

castOp = do
  symbol "CAST"
  t <- typeNote
  v <- varNote
  return $ M.CAST t v

renameOp = do
  symbol "RENAME"
  v <- varNote
  return $ M.RENAME v

concatOp = do
  symbol "CONCAT"
  v <- varNote
  return $ M.CONCAT v

addOp = do
  symbol "ADD"
  v <- varNote
  return $ M.ADD v

subOp = do
  symbol "SUB"
  v <- varNote
  return $ M.SUB v

mulOp = do
  symbol "MUL"
  v <- varNote
  return $ M.MUL v

edivOp = do
  symbol "EDIV"
  v <- varNote
  return $ M.EDIV v

absOp = do
  symbol "ABS"
  v <- varNote
  return $ M.ABS v

negOp = do
  symbol "NEG"
  -- v <- varNote
  return M.NEG

modOp = do
  symbol "MOD"
  -- v <- varNote
  return M.MOD

lslOp = do
  symbol "LSL"
  v <- varNote
  return $ M.LSL v

lsrOp = do
  symbol "LSR"
  v <- varNote
  return $ M.LSR v

orOp = do
  symbol "OR"
  v <- varNote
  return $ M.OR v

andOp = do
  symbol "AND"
  v <- varNote
  return $ M.AND v

notOp = do
  symbol "NOT"
  v <- varNote
  return $ M.NOT v

compareOp = do
  symbol "COMPARE"
  v <- varNote
  return $ M.COMPARE v

eqOp = do
  symbol "EQ"
  v <- varNote
  return $ M.EQ v

neqOp = do
  symbol "NEQ"
  v <- varNote
  return $ M.NEQ v

ltOp = do
  symbol "LT"
  v <- varNote
  return $ M.LT v

gtOp = do
  symbol "GT"
  v <- varNote
  return $ M.GT v

leOp = do
  symbol "LE"
  v <- varNote
  return $ M.LE v

geOp = do
  symbol "GE"
  v <- varNote
  return $ M.GE v

intOp = do
  symbol "INT"
  v <- varNote
  return $ M.INT v

selfOp = do
  symbol "SELF"
  v <- varNote
  return $ M.SELF v

transferTokensOp = do
  symbol "TRANSFER_TOKENS"
  return $ M.TRANSFER_TOKENS

setDelegateOp = do
  symbol "SET_DELEGATE"
  return $ M.SET_DELEGATE

createAccountOp = do
  symbol "CREATE_ACCOUNT"
  v <- varNote
  v' <- varNote
  return $ M.CREATE_ACCOUNT v v'

createContract2Op = do
  symbol "CREATE_CONTRACT"
  v <- varNote
  v' <- varNote
  a <- ops
  return $ M.CREATE_CONTRACT2 v v' a

createContractOp = do
  symbol "CREATE_CONTRACT"
  v <- varNote
  v' <- varNote
  return $ M.CREATE_CONTRACT v v'

implicitAccountOp = do
  symbol "IMPLICIT_ACCOUNT"
  v <- varNote
  return $ M.IMPLICIT_ACCOUNT v

nowOp = do
  symbol "NOW"
  v <- varNote
  return $ M.NOW v

amountOp = do
  symbol "AMOUNT"
  v <- varNote
  return $ M.AMOUNT v

balanceOp = do
  symbol "BALANCE"
  v <- varNote
  return $ M.BALANCE v

checkSignatureOp = do
  symbol "CHECK_SIGNATURE"
  v <- varNote
  return $ M.CHECK_SIGNATURE v

blake2BOp = do
  symbol "BLAKE2B"
  v <- varNote
  return $ M.BLAKE2B v

hashKeyOp = do
  symbol "HASH_KEY"
  v <- varNote
  return $ M.HASH_KEY v

stepsToQuotaOp = do
  symbol "STEPS_TO_QUOTA"
  v <- varNote
  return $ M.STEPS_TO_QUOTA v

sourceOp = do
  symbol "SOURCE"
  v <- varNote
  return $ M.SOURCE v

senderOp = do
  symbol "SENDER"
  v <- varNote
  return $ M.SENDER v

addressOp = do
  symbol "ADDRESS"
  v <- varNote
  return $ M.ADDRESS v

-- Notes

typeNote :: Parser M.TypeNote
typeNote = M.TypeNote <$> ((try $ note ":") <|> pure Nothing)

fieldNote :: Parser M.FieldNote
fieldNote = M.FieldNote <$> ((try $ note "%") <|> pure Nothing)

varNote :: Parser M.VarNote
varNote = M.VarNote <$> ((try $ note "@") <|> pure Nothing)

-- for permutation
typeNote' :: Parser M.TypeNote
typeNote' = M.TypeNote <$> note ":"

fieldNote' :: Parser M.FieldNote
fieldNote' = M.FieldNote <$> note "%"

varNote' :: Parser M.VarNote
varNote' = M.VarNote <$> note "@"

note :: T.Text -> Parser (Maybe T.Text)
note c = Just <$> (lexeme $ string c >> (note' <|> pure ""))
  where
    note' = do
      a <- string "@"
           <|> string "%"
           <|> string "%%"
           <|> T.singleton <$> satisfy isAlphaNum
      let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['\\', '.', '_']
      b <- takeWhileP Nothing (\ x -> x `elem` chars)
      return $ T.append a b

-- Types

type_ :: Parser M.Type
type_ = lexeme $ (ti <|> parens ti)
  where
    ti = typeInner (return M.noFN)

typeInner :: Parser M.FieldNote -> Parser M.Type
typeInner fp = lexeme $ comparableType fp
  <|> keyType fp
  <|> unitType fp
  <|> signatureType fp
  <|> optionType fp
  <|> listType fp
  <|> setType fp
  <|> operationType fp
  <|> addressType fp
  <|> contractType fp
  <|> pairType fp
  <|> orType fp
  <|> mapType fp
  <|> bigMapType fp

field :: Parser M.Type
field = (try $ typeInner fieldNote) <|> (parens $ typeInner fieldNote)

typeAndField :: Parser M.FieldNote -> Parser (M.TypeNote, M.FieldNote)
typeAndField fp = permute2WithDefault typeNote fp M.noTN M.noFN

keyType fp = do
  symbol "key"
  (t, f) <- typeAndField fp
  return $ M.Type (M.T_key) t f

unitType fp = do
  symbol "unit"
  (t, f) <- typeAndField fp
  return $ M.Type M.T_unit t f

signatureType fp = do
  symbol "signature"
  (t, f) <- typeAndField fp
  return $ M.Type M.T_signature t f

optionType fp = do
  symbol "option"
  (t, f) <- typeAndField fp
  a <- field
  return $ M.Type (M.T_option a) t f

listType fp = do
  symbol "list"
  (t, f) <- typeAndField fp
  a <- type_
  return $ M.Type (M.T_list a) t f

setType fp = do
  symbol "set"
  (t, f) <- typeAndField fp
  a <- comparable
  return $ M.Type (M.T_set a) t f

operationType fp = do
  symbol "operation"
  (t, f) <- typeAndField fp
  return $ M.Type M.T_operation t f

addressType fp = do
  symbol "address"
  (t, f) <- typeAndField fp
  return $ M.Type M.T_address t f

contractType fp = do
  symbol "contract"
  (t, f) <- typeAndField fp
  a <- type_
  return $ M.Type (M.T_contract a) t f

--pairType :: Parser M.FieldNote -> Parser M.Type
pairType fp = do
  symbol "pair"
  (t, f) <- typeAndField fp
  a <- field
  b <- field
  return $ M.Type (M.T_pair a b) t f

orType fp = do
  symbol "or"
  (t, f) <- typeAndField fp
  a <- field
  b <- field
  return $ M.Type (M.T_or a b) t f

lambdaType fp = do
  symbol "lambda"
  (t, f) <- typeAndField fp
  a <- type_
  b <- type_
  return $ M.Type (M.T_lambda a b) t f

mapType fp = do
  symbol "map"
  (t, f) <- typeAndField fp
  a <- comparable
  b <- type_
  return $ M.Type (M.T_map a b) t f

bigMapType fp = do
  symbol "big_map"
  (t, f) <- typeAndField fp
  a <- comparable
  b <- type_
  return $ M.Type (M.T_map a b) t f

-- Comparable Types
comparableType :: Parser M.FieldNote -> Parser M.Type
comparableType fp = do
  ct <- ct
  (t, f) <- typeAndField fp
  return $ (M.Type (M.T_comparable ct) t f)

comparable :: Parser M.Comparable
comparable = do
  ct <- ct
  t <- typeNote
  return $ M.Comparable ct t

ct :: Parser M.CT
ct = (symbol "int" >> return M.T_int)
  <|> (symbol "nat" >> return M.T_nat)
  <|> (symbol "string" >> return M.T_string)
  <|> (symbol "bytes" >> return M.T_bytes)
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
  <|> cadrMac

cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp


cmpMac :: Parser M.Ops
cmpMac = do
  string "CMP"
  a <- cmpOp
  return $ cmpX a

cmpX :: M.Op -> M.Ops
cmpX op = M.opsFromList [M.COMPARE M.noVN, op]

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
ifcmpX op bt bf = M.opsFromList [M.COMPARE M.noVN, op, M.IF bt bf]

-- fail

failMac :: Parser M.Ops
failMac = symbol "FAIL" >> return failX

failX :: M.Ops
failX = M.opsFromList [M.UNIT M.noTN, M.FAILWITH]

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
dupX n = go n (M.opsFromList [M.DIP $ M.opsFromList [(M.DUP M.noVN)], M.SWAP])
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

pairN = (M.PAIR M.noTN M.noVN M.noFN M.noFN)

--unpair
unpairMac :: Parser M.Ops
unpairMac = unpairX <$> (string "UN" >> pairMacInner <* symbol "R")

unpairX :: P -> M.Ops
unpairX (Nest Stop Stop) =
  (M.|:) [M.DUP M.noVN, M.CAR M.noVN M.noFN, M.DIP $ (M.|:) [M.CDR M.noVN M.noFN]]
unpairX (Nest Stop r) = unpairN M.>< (M.|:) [M.DIP $ unpairX r]
unpairX (Nest l Stop) = unpairN M.>< (unpairX l)
unpairX (Nest l r) = (unpairN) M.>< (unpairX l) M.>< (unpairX r)

unpairN = unpairX (Nest Stop Stop)

cadrMac :: Parser M.Ops
cadrMac = lexeme $ do
  string "C"
  a <- some $ try $ cadrInner <* (notFollowedBy $ string "R")
  let a' = a <*> pure M.noVN <*> pure M.noFN
  b <- cadrInner
  symbol "R"
  (vn, fn) <- varAndField
  return $ (M.|:) a' M.|> (b vn fn)
  where
    cadrInner = (string "A" >> return M.CAR)
            <|> (string "D" >> return M.CDR)

--
--if_some
--
--set_cadr
--
--map_cadr
--
--
