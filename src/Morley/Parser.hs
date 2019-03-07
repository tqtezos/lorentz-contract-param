module Morley.Parser
  ( contract
  , ops
  , ParserException (..)
  , stringLiteral
  , type_
  , value
  , stackType
  , printComment
  ) where

import Prelude hiding (many, note, some, try)

import Control.Applicative.Permutations (intercalateEffect, toPermutation)
import qualified Data.ByteString.Base16 as B16
import Data.Char as Char
import Data.Default (Default)
import qualified Data.Text as T

import Text.Megaparsec
  (choice, customFailure, eitherP, many, manyTill, notFollowedBy, satisfy, sepEndBy, some,
   takeWhile1P, try)
import Text.Megaparsec.Char (alphaNumChar, char, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Morley.Lexer
import qualified Morley.Macro as Macro
import Morley.Parser.Annotations
import Morley.Types (CustomParserException(..), ParsedOp(..), Parser, ParserException(..))
import qualified Morley.Types as M

-------------------------------------------------------------------------------
-- Top-Level Parsers
-------------------------------------------------------------------------------

contract :: Parser (M.Contract ParsedOp)
contract = do
  mSpace
  (p,s,c) <- intercalateEffect semicolon $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ M.Contract p s c

parameter :: Parser M.Type
parameter = do void $ symbol "parameter"; type_

storage :: Parser M.Type
storage = do void $ symbol "storage"; type_

code :: Parser [ParsedOp]
code = do void $ symbol "code"; ops

value :: Parser (M.Value ParsedOp)
value = lexeme $ valueInner <|> parens valueInner

type_ :: Parser M.Type
type_ = (ti <|> parens ti) <|> (customFailure UnknownTypeException)
  where
    ti = snd <$> (lexeme $ typeInner (pure M.noAnn))

ops :: Parser [M.ParsedOp]
ops = braces $ sepEndBy op' semicolon
  where
    op' = choice
      [ (M.PRIM . M.NOP) <$> nopInstr
      , M.PRIM <$> prim
      , M.MAC <$> macro
      , primOrMac
      , M.SEQ <$> ops
      ]

-------------------------------------------------------------------------------
-- Value Parsers
-------------------------------------------------------------------------------

valueInner :: Parser (M.Value M.ParsedOp)
valueInner = choice $
  [ intLiteral, stringLiteral, bytesLiteral, unitValue
  , trueValue, falseValue, pairValue, leftValue, rightValue
  , someValue, noneValue, seqValue, mapValue, lambdaValue
  ]

-- Literals
intLiteral :: Parser (M.Value a)
intLiteral = try $ M.ValueInt <$> (L.signed (return ()) L.decimal)

bytesLiteral :: Parser (M.Value a)
bytesLiteral = try $ do
  symbol "0x"
  hexdigits <- takeWhile1P Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return . M.ValueBytes . M.InternalByteString $ bytes
  else customFailure OddNumberBytesException

stringLiteral :: Parser (M.Value ParsedOp)
stringLiteral = try $ M.ValueString <$>
  (T.pack <$>
    ( (++) <$>
        (concat <$> (string "\"" >> many validChar)) <*>
        (manyTill (lineBreakChar <|> (customFailure $ UnexpectedLineBreak)) (string "\""))
    )
  )
  where
      validChar :: Parser String
      validChar =
        try strEscape <|>
          try ((:[]) <$> satisfy (\x -> x /= '"' && x /= '\n' && x /= '\r'))
      lineBreakChar :: Parser Char
      lineBreakChar = char '\n' <|> char '\r'

strEscape :: Parser String
strEscape = char '\\' >> esc
  where
    esc = (char 't' >> return "\t")
      <|> (char 'b' >> return "\b")
      <|> (char '\\' >> return "\\")
      <|> (char '"' >> return "\"")
      <|> (char 'n' >> return "\n")
      <|> (char 'r' >> return "\r")


unitValue :: Parser (M.Value ParsedOp)
unitValue = do symbol "Unit"; return M.ValueUnit

trueValue :: Parser (M.Value ParsedOp)
trueValue = do symbol "True"; return M.ValueTrue

falseValue :: Parser (M.Value ParsedOp)
falseValue = do symbol "False"; return M.ValueFalse

pairValue :: Parser (M.Value ParsedOp)
pairValue = core <|> tuple
  where
    core = do symbol "Pair"; a <- value; M.ValuePair a <$> value
    tuple = try $ do
      symbol "("
      a <- value
      comma
      b <- tupleInner <|> value
      symbol ")"
      return $ M.ValuePair a b
    tupleInner = try $ do
      a <- value
      comma
      b <- tupleInner <|> value
      return $ M.ValuePair a b

leftValue :: Parser (M.Value ParsedOp)
leftValue = do void $ symbol "Left"; M.ValueLeft <$> value

rightValue :: Parser (M.Value ParsedOp)
rightValue = do void $ symbol "Right"; M.ValueRight <$> value

someValue :: Parser (M.Value ParsedOp)
someValue = do void $ symbol "Some"; M.ValueSome <$> value

noneValue :: Parser (M.Value ParsedOp)
noneValue = do symbol "None"; return M.ValueNone

lambdaValue :: Parser (M.Value ParsedOp)
lambdaValue = M.ValueLambda <$> ops

seqValue :: Parser (M.Value ParsedOp)
seqValue = M.ValueSeq <$> (try $ braces $ sepEndBy value semicolon)

eltValue :: Parser (M.Elt ParsedOp)
eltValue = do void $ symbol "Elt"; M.Elt <$> value <*> value

mapValue :: Parser (M.Value ParsedOp)
mapValue = M.ValueMap <$> (try $ braces $ sepEndBy eltValue semicolon)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
field :: Parser (M.FieldAnn, M.Type)
field = lexeme (fi <|> parens fi)
  where
    fi = typeInner noteF

typeInner :: Parser M.FieldAnn -> Parser (M.FieldAnn, M.Type)
typeInner fp = choice $ (\x -> x fp) <$>
  [ t_ct, t_key, t_unit, t_signature, t_option, t_list, t_set
  , t_operation, t_contract, t_pair, t_or, t_lambda, t_map, t_big_map]

-- Comparable Types
comparable :: Parser M.Comparable
comparable = let c = do ct' <- ct; M.Comparable ct' <$> noteTDef in parens c <|> c

t_ct :: (Default a) => Parser a -> Parser (a, M.Type)
t_ct fp = do ct' <- ct; (f,t) <- fieldType fp; return (f, M.Type (M.T_comparable ct') t)

ct :: Parser M.CT
ct = (symbol "int" >> return M.T_int)
  <|> (symbol "nat" >> return M.T_nat)
  <|> (symbol "string" >> return M.T_string)
  <|> (symbol "bytes" >> return M.T_bytes)
  <|> (symbol "mutez" >> return M.T_mutez)
  <|> (symbol "bool" >> return M.T_bool)
  <|> (symbol "key_hash" >> return M.T_key_hash)
  <|> (symbol "timestamp" >> return M.T_timestamp)
  <|> (symbol "address" >> return M.T_address)

-- Protocol Types
t_key :: (Default a) => Parser a -> Parser (a, M.Type)
t_key       fp = do symbol "key"; (f,t) <- fieldType fp; return (f, M.Type M.T_key t)

t_signature :: (Default a) => Parser a -> Parser (a, M.Type)
t_signature fp = do symbol "signature"; (f, t) <- fieldType fp; return (f, M.Type M.T_signature t)

t_operation :: (Default a) => Parser a -> Parser (a, M.Type)
t_operation fp = do symbol "operation"; (f, t) <- fieldType fp; return (f, M.Type M.T_operation t)

t_contract :: (Default a) => Parser a -> Parser (a, M.Type)
t_contract  fp = do symbol "contract"; (f, t) <- fieldType fp; a <- type_; return (f, M.Type (M.T_contract a) t)
--(do symbol "address"; (f, t) <- ft; return (f, M.Type M.T_address t)

-- Abstraction Types
t_unit :: (Default a) => Parser a -> Parser (a, M.Type)
t_unit fp = do
  symbol "unit" <|> symbol "()"
  (f,t) <- fieldType fp
  return (f, M.Type M.T_unit t)

t_pair :: (Default a) => Parser a -> Parser (a, M.Type)
t_pair fp = core <|> tuple
  where
    core = do
      symbol "pair"
      (f, t) <- fieldType fp
      (l, a) <- field
      (r, b) <- field
      return (f, M.Type (M.T_pair l r a b) t)
    tuple = try $ do
      symbol "("
      (l, a) <- field
      comma
      (r, b) <- tupleInner <|> field
      symbol ")"
      (f, t) <- fieldType fp
      return (f, M.Type (M.T_pair l r a b) t)
    tupleInner = try $ do
      (l, a) <- field
      comma
      (r, b) <- tupleInner <|> field
      return (M.noAnn, M.Type (M.T_pair l r a b) M.noAnn)

t_or :: (Default a) => Parser a -> Parser (a, M.Type)
t_or fp = core <|> bar
  where
    core = do
      symbol "or"
      (f, t) <- fieldType fp
      (l, a) <- field
      (r, b) <- field
      return (f, M.Type (M.T_or l r a b) t)
    bar = try $ do
      symbol "("
      (l, a) <- field
      symbol "|"
      (r, b) <- barInner <|> field
      symbol ")"
      (f, t) <- fieldType fp
      return (f, M.Type (M.T_or l r a b) t)
    barInner = try $ do
      (l, a) <- field
      symbol "|"
      (r, b) <- barInner <|> field
      return (M.noAnn, M.Type (M.T_or l r a b) M.noAnn)

t_option :: (Default a) => Parser a -> Parser (a, M.Type)
t_option fp = do
  symbol "option"
  (f, t) <- fieldType fp
  (fa, a) <- field
  return (f, M.Type (M.T_option fa a) t)

t_lambda :: (Default a) => Parser a -> Parser (a, M.Type)
t_lambda fp = core <|> slashLambda
  where
    core = do
      symbol "lambda"
      (f, t) <- fieldType fp
      a <- type_
      b <- type_
      return (f, M.Type (M.T_lambda a b) t)
    slashLambda = do
      symbol "\\"
      (f, t) <- fieldType fp
      a <- type_
      symbol "->"
      b <- type_
      return (f, M.Type (M.T_lambda a b) t)

-- Container types
t_list :: (Default a) => Parser a -> Parser (a, M.Type)
t_list fp = core <|> bracketList
  where
    core = do
      symbol "list"
      (f, t) <- fieldType fp
      a <- type_
      return (f, M.Type (M.T_list a) t)
    bracketList = do
      a <- brackets type_
      (f, t) <- fieldType fp
      return (f, M.Type (M.T_list a) t)

t_set :: (Default a) => Parser a -> Parser (a, M.Type)
t_set fp = core <|> braceSet
  where
    core = do
      symbol "set"
      (f, t) <- fieldType fp
      a <- comparable
      return (f, M.Type (M.T_set a) t)
    braceSet = do
      a <- braces comparable
      (f, t) <- fieldType fp
      return (f, M.Type (M.T_set a) t)

t_map :: (Default a) => Parser a -> Parser (a, M.Type)
t_map fp = (do symbol "map"; (f, t) <- fieldType fp; a <- comparable; b <- type_; return (f, M.Type (M.T_map a b) t))

t_big_map :: (Default a) => Parser a -> Parser (a, M.Type)
t_big_map fp = (do symbol "big_map"; (f, t) <- fieldType fp; a <- comparable; b <- type_; return (f, M.Type (M.T_big_map a b) t))

-------------------------------------------------------------------------------
-- Primitive Instruction Parsers
-------------------------------------------------------------------------------
prim :: Parser M.ParsedInstr
prim = choice
  [ dropOp, dupOp, swapOp, pushOp, someOp, noneOp, unitOp, ifNoneOp
  , carOp, cdrOp, leftOp, rightOp, ifLeftOp, ifRightOp, nilOp, consOp, ifConsOp
  , sizeOp, emptySetOp, emptyMapOp, iterOp, memOp, getOp, updateOp
  , loopLOp, loopOp, lambdaOp, execOp, dipOp, failWithOp, castOp, renameOp
  , concatOp, packOp, unpackOp, sliceOp, isNatOp, addressOp, addOp, subOp
  , mulOp, edivOp, absOp, negOp, lslOp, lsrOp, orOp, andOp, xorOp, notOp
  , compareOp, eqOp, neqOp, ltOp, leOp, gtOp, geOp, intOp, selfOp, contractOp
  , transferTokensOp, setDelegateOp, createAccountOp, createContract2Op
  , createContractOp, implicitAccountOp, nowOp, amountOp, balanceOp, checkSigOp
  , sha256Op, sha512Op, blake2BOp, hashKeyOp, stepsToQuotaOp, sourceOp, senderOp
  ]

-- Control Structures

failWithOp :: Parser M.ParsedInstr
failWithOp = do symbol' "FAILWITH"; return M.FAILWITH

loopOp :: Parser M.ParsedInstr
loopOp  = do void $ symbol' "LOOP"; M.LOOP <$> ops

loopLOp :: Parser M.ParsedInstr
loopLOp = do void $ symbol' "LOOP_LEFT"; M.LOOP_LEFT <$> ops

execOp :: Parser M.ParsedInstr
execOp = do void $ symbol' "EXEC"; M.EXEC <$> noteVDef

dipOp :: Parser M.ParsedInstr
dipOp = do void $ symbol' "DIP"; M.DIP <$> ops

-- Stack Operations

dropOp :: Parser M.ParsedInstr
dropOp = do symbol' "DROP"; return M.DROP;

dupOp :: Parser M.ParsedInstr
dupOp = do void $ symbol' "DUP"; M.DUP <$> noteVDef

swapOp :: Parser M.ParsedInstr
swapOp = do symbol' "SWAP"; return M.SWAP;

pushOp :: Parser M.ParsedInstr
pushOp = do symbol' "PUSH"; v <- noteVDef; a <- type_; M.PUSH v a <$> value

unitOp :: Parser M.ParsedInstr
unitOp = do symbol' "UNIT"; (t, v) <- notesTV; return $ M.UNIT t v

lambdaOp :: Parser M.ParsedInstr
lambdaOp = do symbol' "LAMBDA"; v <- noteVDef; a <- type_; b <- type_;
              M.LAMBDA v a b <$> ops

-- Generic comparison

eqOp :: Parser M.ParsedInstr
eqOp = do void $ symbol' "EQ"; M.EQ <$> noteVDef

neqOp :: Parser M.ParsedInstr
neqOp = do void $ symbol' "NEQ"; M.NEQ <$> noteVDef

ltOp :: Parser M.ParsedInstr
ltOp = do void $ symbol' "LT"; M.LT <$> noteVDef

gtOp :: Parser M.ParsedInstr
gtOp = do void $ symbol' "GT"; M.GT <$> noteVDef

leOp :: Parser M.ParsedInstr
leOp = do void $ symbol' "LE"; M.LE <$> noteVDef

geOp :: Parser M.ParsedInstr
geOp = do void $ symbol' "GE"; M.GE <$> noteVDef

-- ad-hoc comparison

compareOp :: Parser M.ParsedInstr
compareOp = do void $ symbol' "COMPARE"; M.COMPARE <$> noteVDef

-- Operations on booleans

orOp :: Parser M.ParsedInstr
orOp = do void $ symbol' "OR";  M.OR <$> noteVDef

andOp :: Parser M.ParsedInstr
andOp = do void $ symbol' "AND"; M.AND <$> noteVDef

xorOp :: Parser M.ParsedInstr
xorOp = do void $ symbol' "XOR"; M.XOR <$> noteVDef

notOp :: Parser M.ParsedInstr
notOp = do void $ symbol' "NOT"; M.NOT <$> noteVDef

-- Operations on integers and natural numbers

addOp :: Parser M.ParsedInstr
addOp = do void $ symbol' "ADD"; M.ADD <$> noteVDef

subOp :: Parser M.ParsedInstr
subOp = do void $ symbol' "SUB"; M.SUB <$> noteVDef

mulOp :: Parser M.ParsedInstr
mulOp = do void $ symbol' "MUL"; M.MUL <$> noteVDef

edivOp :: Parser M.ParsedInstr
edivOp = do void $ symbol' "EDIV";M.EDIV <$> noteVDef

absOp :: Parser M.ParsedInstr
absOp = do void $ symbol' "ABS"; M.ABS <$> noteVDef

negOp :: Parser M.ParsedInstr
negOp = do symbol' "NEG"; return M.NEG;

-- Bitwise logical operators

lslOp :: Parser M.ParsedInstr
lslOp = do void $ symbol' "LSL"; M.LSL <$> noteVDef

lsrOp :: Parser M.ParsedInstr
lsrOp = do void $ symbol' "LSR"; M.LSR <$> noteVDef

-- Operations on string's

concatOp :: Parser M.ParsedInstr
concatOp = do void $ symbol' "CONCAT"; M.CONCAT <$> noteVDef

sliceOp :: Parser M.ParsedInstr
sliceOp = do void $ symbol' "SLICE"; M.SLICE <$> noteVDef

-- Operations on pairs
pairOp :: Parser M.ParsedInstr
pairOp = do symbol' "PAIR"; (t, v, (p, q)) <- notesTVF2; return $ M.PAIR t v p q

carOp :: Parser M.ParsedInstr
carOp = do symbol' "CAR"; (v, f) <- notesVF; return $ M.CAR v f

cdrOp :: Parser M.ParsedInstr
cdrOp = do symbol' "CDR"; (v, f) <- notesVF; return $ M.CDR v f

-- Operations on collections (sets, maps, lists)

emptySetOp :: Parser M.ParsedInstr
emptySetOp = do symbol' "EMPTY_SET"; (t, v) <- notesTV;
                M.EMPTY_SET t v <$> comparable

emptyMapOp :: Parser M.ParsedInstr
emptyMapOp = do symbol' "EMPTY_MAP"; (t, v) <- notesTV; a <- comparable;
                M.EMPTY_MAP t v a <$> type_

memOp :: Parser M.ParsedInstr
memOp = do void $ symbol' "MEM"; M.MEM <$> noteVDef

updateOp :: Parser M.ParsedInstr
updateOp = do symbol' "UPDATE"; return M.UPDATE

iterOp :: Parser M.ParsedInstr
iterOp = do void $ symbol' "ITER"; M.ITER <$> ops

sizeOp :: Parser M.ParsedInstr
sizeOp = do void $ symbol' "SIZE"; M.SIZE <$> noteVDef

mapOp :: Parser M.ParsedInstr
mapOp = do symbol' "MAP"; v <- noteVDef; M.MAP v <$> ops

getOp :: Parser M.ParsedInstr
getOp = do void $ symbol' "GET"; M.GET <$> noteVDef

nilOp :: Parser M.ParsedInstr
nilOp = do symbol' "NIL"; (t, v) <- notesTV; M.NIL t v <$> type_

consOp :: Parser M.ParsedInstr
consOp = do void $ symbol' "CONS"; M.CONS <$> noteVDef

ifConsOp :: Parser M.ParsedInstr
ifConsOp = do symbol' "IF_CONS"; a <- ops; M.IF_CONS a <$> ops

-- Operations on options

someOp :: Parser M.ParsedInstr
someOp = do symbol' "SOME"; (t, v, f) <- notesTVF; return $ M.SOME t v f

noneOp :: Parser M.ParsedInstr
noneOp = do symbol' "NONE"; (t, v, f) <- notesTVF; M.NONE t v f <$> type_

ifNoneOp :: Parser M.ParsedInstr
ifNoneOp = do symbol' "IF_NONE"; a <- ops; M.IF_NONE a <$> ops

-- Operations on unions

leftOp :: Parser M.ParsedInstr
leftOp = do symbol' "LEFT"; (t, v, (f, f')) <- notesTVF2;
               M.LEFT t v f f' <$> type_

rightOp :: Parser M.ParsedInstr
rightOp = do symbol' "RIGHT"; (t, v, (f, f')) <- notesTVF2;
               M.RIGHT t v f f' <$> type_

ifLeftOp :: Parser M.ParsedInstr
ifLeftOp = do symbol' "IF_LEFT"; a <- ops; M.IF_LEFT a <$> ops

ifRightOp :: Parser M.ParsedInstr
ifRightOp = do symbol' "IF_RIGHT"; a <- ops; M.IF_RIGHT a <$> ops

-- Operations on contracts

createContractOp :: Parser M.ParsedInstr
createContractOp = do symbol' "CREATE_CONTRACT"; v <- noteVDef;
                       M.CREATE_CONTRACT v <$> noteVDef

createContract2Op :: Parser M.ParsedInstr
createContract2Op = do symbol' "CREATE_CONTRACT"; v <- noteVDef; v' <- noteVDef;
                       M.CREATE_CONTRACT2 v v' <$> braces contract

createAccountOp :: Parser M.ParsedInstr
createAccountOp = do symbol' "CREATE_ACCOUNT"; v <- noteVDef; v' <- noteVDef;
                       return $ M.CREATE_ACCOUNT v v'

transferTokensOp :: Parser M.ParsedInstr
transferTokensOp = do void $ symbol' "TRANSFER_TOKENS"; M.TRANSFER_TOKENS <$> noteVDef

setDelegateOp :: Parser M.ParsedInstr
setDelegateOp = do void $ symbol' "SET_DELEGATE"; M.SET_DELEGATE <$> noteVDef

balanceOp :: Parser M.ParsedInstr
balanceOp = do void $ symbol' "BALANCE"; M.BALANCE <$> noteVDef

contractOp :: Parser M.ParsedInstr
contractOp = do void $ symbol' "CONTRACT"; M.CONTRACT <$> noteVDef <*> type_

sourceOp :: Parser M.ParsedInstr
sourceOp = do void $ symbol' "SOURCE"; M.SOURCE <$> noteVDef

senderOp :: Parser M.ParsedInstr
senderOp = do void $ symbol' "SENDER"; M.SENDER <$> noteVDef

amountOp :: Parser M.ParsedInstr
amountOp = do void $ symbol' "AMOUNT"; M.AMOUNT <$> noteVDef

implicitAccountOp :: Parser M.ParsedInstr
implicitAccountOp = do void $ symbol' "IMPLICIT_ACCOUNT"; M.IMPLICIT_ACCOUNT <$> noteVDef

selfOp :: Parser M.ParsedInstr
selfOp = do void $ symbol' "SELF"; M.SELF <$> noteVDef

addressOp :: Parser M.ParsedInstr
addressOp = do void $ symbol' "ADDRESS"; M.ADDRESS <$> noteVDef

-- Special Operations
nowOp :: Parser M.ParsedInstr
nowOp = do void $ symbol' "NOW"; M.NOW <$> noteVDef

stepsToQuotaOp :: Parser M.ParsedInstr
stepsToQuotaOp = do void $ symbol' "STEPS_TO_QUOTA"; M.STEPS_TO_QUOTA <$> noteVDef

-- Operations on bytes
packOp :: Parser M.ParsedInstr
packOp = do void $ symbol' "PACK"; M.PACK <$> noteVDef

unpackOp :: Parser M.ParsedInstr
unpackOp = do symbol' "UNPACK"; v <- noteVDef; M.UNPACK v <$> type_

-- Cryptographic Primitives

checkSigOp :: Parser M.ParsedInstr
checkSigOp = do void $ symbol' "CHECK_SIGNATURE"; M.CHECK_SIGNATURE <$> noteVDef

blake2BOp :: Parser M.ParsedInstr
blake2BOp = do void $ symbol' "BLAKE2B"; M.BLAKE2B <$> noteVDef

sha256Op :: Parser M.ParsedInstr
sha256Op = do void $ symbol' "SHA256"; M.SHA256 <$> noteVDef

sha512Op :: Parser M.ParsedInstr
sha512Op = do void $ symbol' "SHA512"; M.SHA512 <$> noteVDef

hashKeyOp :: Parser M.ParsedInstr
hashKeyOp = do void $ symbol' "HASH_KEY"; M.HASH_KEY <$> noteVDef

{- Type operations -}
castOp :: Parser M.ParsedInstr
castOp = do void $ symbol' "CAST"; M.CAST <$> noteVDef <*> type_;

renameOp :: Parser M.ParsedInstr
renameOp = do void $ symbol' "RENAME"; M.RENAME <$> noteVDef

isNatOp :: Parser M.ParsedInstr
isNatOp = do void $ symbol' "ISNAT"; M.ISNAT <$> noteVDef

intOp :: Parser M.ParsedInstr
intOp = do void $ symbol' "INT"; M.INT <$> noteVDef

-------------------------------------------------------------------------------
-- Macro Parsers
-------------------------------------------------------------------------------
cmpOp :: Parser M.ParsedInstr
cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp <|> geOp

macro :: Parser M.Macro
macro = do symbol' "CMP"; a <- cmpOp; M.CMP a <$> noteVDef
  <|> do symbol' "IF_SOME"; a <- ops; M.IF_SOME a <$> ops
  <|> do symbol' "FAIL"; return M.FAIL
  <|> do void $ symbol' "ASSERT_CMP"; M.ASSERT_CMP <$> cmpOp
  <|> do symbol' "ASSERT_NONE"; return M.ASSERT_NONE
  <|> do symbol' "ASSERT_SOME"; return M.ASSERT_SOME
  <|> do symbol' "ASSERT_LEFT"; return M.ASSERT_LEFT
  <|> do symbol' "ASSERT_RIGHT"; return M.ASSERT_RIGHT
  <|> do void $ symbol' "ASSERT_"; M.ASSERTX <$> cmpOp
  <|> do symbol' "ASSERT"; return M.ASSERT
  <|> do string' "DI"; n <- num "I"; symbol' "P"; M.DIIP (n + 1) <$> ops
  <|> do string' "DU"; n <- num "U"; symbol' "P"; M.DUUP (n + 1) <$> noteVDef
  <|> unpairMac
  <|> cadrMac
  <|> setCadrMac
  where
   num str = fromIntegral . length <$> some (string' str)

pairMac :: Parser M.Macro
pairMac = do
  a <- pairMacInner
  symbol' "R"
  (tn, vn, fns) <- permute3Def noteTDef noteV (some noteF)
  let ps = Macro.mapLeaves ((M.noAnn,) <$> fns) a
  return $ M.PAPAIR ps tn vn

pairMacInner :: Parser M.PairStruct
pairMacInner = do
  string' "P"
  l <- (string' "A" >> return (M.F (M.noAnn, M.noAnn))) <|> pairMacInner
  r <- (string' "I" >> return (M.F (M.noAnn, M.noAnn))) <|> pairMacInner
  return $ M.P l r

unpairMac :: Parser M.Macro
unpairMac = do
  string' "UN"
  a <- pairMacInner
  symbol' "R"
  (vns, fns) <- permute2Def (some noteV) (some noteF)
  return $ M.UNPAIR (Macro.mapLeaves (zip vns fns) a)

cadrMac :: Parser M.Macro
cadrMac = lexeme $ do
  string' "C"
  a <- some $ try $ cadrInner <* notFollowedBy (string' "R")
  b <- cadrInner
  symbol' "R"
  (vn, fn) <- notesVF
  return $ M.CADR (a ++ pure b) vn fn

cadrInner :: Parser M.CadrStruct
cadrInner = (string' "A" >> return M.A) <|> (string' "D" >> return M.D)

setCadrMac :: Parser M.Macro
setCadrMac = do
  string' "SET_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  return $ M.SET_CADR a v f

mapCadrMac :: Parser M.Macro
mapCadrMac = do
  string' "MAP_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  M.MAP_CADR a v f <$> ops

ifCmpMac :: Parser M.Macro
ifCmpMac = symbol' "IFCMP" >> M.IFCMP <$> cmpOp <*> noteVDef <*> ops <*> ops

ifOrIfX :: Parser M.ParsedOp
ifOrIfX = do
  symbol' "IF"
  a <- eitherP cmpOp ops
  case a of
    Left cmp -> M.MAC <$> (M.IFX cmp <$> ops <*> ops)
    Right op -> M.PRIM <$> (M.IF op <$> ops)

-- Some of the operations and macros have the same prefixes in their names
-- So this case should be handled separately
primOrMac :: Parser M.ParsedOp
primOrMac = ((M.MAC <$> ifCmpMac) <|> ifOrIfX)
  <|> ((M.MAC <$> mapCadrMac) <|> (M.PRIM <$> mapOp))
  <|> (try (M.PRIM <$> pairOp) <|> M.MAC <$> pairMac)

-------------------------------------------------------------------------------
-- Morley Instructions
-------------------------------------------------------------------------------

nopInstr :: Parser M.NopInstr
nopInstr = choice [stackOp, testOp, printOp]

stackOp :: Parser M.NopInstr
stackOp = symbol' "STACKTYPE" >> M.STACKTYPE <$> stackType

testOp :: Parser M.NopInstr
testOp = symbol' "TEST" >> M.TEST <$> test

printOp :: Parser M.NopInstr
printOp = symbol' "PRINT" >> M.PRINT <$> printComment

test :: Parser M.InlineTest
test = do
  n <- lexeme (T.pack <$> some alphaNumChar)
  c <- printComment
  o <- ops
  return $ M.InlineTest n c o

printComment :: Parser M.PrintComment
printComment = do
  symbol "\""
  let validChar = T.pack <$> some (satisfy (\x -> x /= '%' && x /= '"'))
  c <- many (Right <$> stackRef <|> Left <$> validChar)
  symbol "\""
  return $ M.PrintComment c

stackRef :: Parser M.StackRef
stackRef = do
  symbol "%"
  n <- brackets L.decimal
  return $ M.StackRef n

stackType :: Parser M.StackTypePattern
stackType = symbol "'[" >> (emptyStk <|> stkCons <|> stkRest)
  where
    emptyStk = try $ symbol "]" >> return M.StkEmpty
    stkRest = try $ symbol "..." >> symbol "]" >> return M.StkRest
    stkCons = try $ do
      t <- type_
      s <- (symbol "," >> stkCons <|> stkRest) <|> emptyStk
      return $ M.StkCons t s
