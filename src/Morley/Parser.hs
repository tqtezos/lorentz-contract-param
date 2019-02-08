{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Morley.Parser
  ( contract
  , ParserException(..)
  ) where

import Prelude hiding (many, note, some, try)

import Control.Applicative.Permutations
import qualified Data.ByteString.Base16 as B16
import Data.Char as Char
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char hiding (string')
import qualified Text.Megaparsec.Char.Lexer as L

import Morley.Lexer
import qualified Morley.Macro as Macro
import Morley.Parser.Annotations
import Morley.Types (ParsedOp(..), Parser, ParserException(..))
import qualified Morley.Types as M

-------------------------------------------------------------------------------
-- Top Level Parsers
-------------------------------------------------------------------------------

contract :: Parser (M.Contract ParsedOp)
contract = do
  mSpace
  (p,s,c) <- intercalateEffect semicolon $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ M.Contract p s c

parameter = do symbol "parameter"; type_
storage   = do symbol "storage"; type_
code      = do symbol "code"; ops

{- Value Parsers -}
data_ :: Parser (M.Value ParsedOp)
data_ = lexeme $ dataInner <|> parens dataInner
  where
    dataInner :: Parser (M.Value M.ParsedOp)
    dataInner = choice $
      [ intLiteral, stringLiteral, bytesLiteral, unitValue
      , trueValue, falseValue, pairValue, leftValue, rightValue
      , someValue, noneValue, seqValue, mapValue, dataOps
      ]

-- Literals
intLiteral = try $ M.Int <$> (L.signed (return ()) L.decimal)
bytesLiteral = try $ do
  symbol "0x"
  hexdigits <- takeWhile1P Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return $ M.Bytes bytes
  else error "odd number bytes" -- TODO: better errors

-- this parses more escape sequences than are in the michelson spec
-- should investigate which sequences this matters for, e.g. \xa == \n
stringLiteral = try $ M.String <$>
  (T.pack <$> (char '"' >> manyTill L.charLiteral (char '"')))

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
unitValue = do symbol "Unit"; return M.Unit
trueValue = do symbol "True"; return M.True
falseValue = do symbol "False"; return M.False
pairValue = core <|> tuple
  where
    core = try $ do symbol "Pair"; a <- data_; M.Pair a <$> data_
    tuple = try $ do
      symbol "("
      a <- data_
      comma
      b <- tupleInner <|> data_
      symbol ")"
      return $ M.Pair a b
    tupleInner = try $ do
      a <- data_
      comma
      b <- tupleInner <|> data_
      return $ M.Pair a b

leftValue = do symbol "Left"; M.Left <$> data_
rightValue = do symbol "Right"; M.Right <$> data_
someValue = do symbol "Some"; M.Some <$> data_
noneValue = do symbol "None"; return M.None
dataOps = M.DataOps <$> ops
seqValue = M.Seq <$> (braces $ sepEndBy data_ semicolon)
eltValue = do symbol "Elt"; M.Elt <$> data_ <*> data_
mapValue = M.Map <$> (braces $ sepEndBy eltValue semicolon)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

field :: Parser (M.FieldNote, M.Type)
field = lexeme (fi <|> parens fi)
  where
    fi = typeInner noteF


type_ :: Parser M.Type
type_ = (ti <|> parens ti)
  where
    ti = snd <$> (lexeme $ typeInner (pure Nothing))

typeInner :: Parser M.FieldNote -> Parser (M.FieldNote, M.Type)
typeInner fp = choice $ (\x -> x fp) <$>
  [ t_ct, t_key, t_unit, t_signature, t_option, t_list, t_set
  , t_operation, t_contract, t_pair, t_or, t_lambda, t_map, t_big_map]

-- Comparable Types
comparable :: Parser M.Comparable
comparable = let c = do ct' <- ct; M.Comparable ct' <$> noteTDef in parens c <|> c

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
t_key       fp = do symbol "key"; (f,t) <- fieldType fp; return (f, M.Type M.T_key t)
t_signature fp = do symbol "signature"; (f, t) <- fieldType fp; return (f, M.Type M.T_signature t)
t_operation fp = do symbol "operation"; (f, t) <- fieldType fp; return (f, M.Type M.T_operation t)
t_contract  fp = do symbol "contract"; (f, t) <- fieldType fp; a <- type_; return (f, M.Type (M.T_contract a) t)
--(do symbol "address"; (f, t) <- ft; return (f, M.Type M.T_address t)

-- Abstraction Types
t_unit fp = do
  symbol "unit" <|> symbol "()"
  (f,t) <- fieldType fp
  return (f, M.Type M.T_unit t)

t_pair fp = core <|> tuple
  where
    core = try $ do
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
      return (Nothing, M.Type (M.T_pair l r a b) Nothing)

t_or fp = core <|> bar
  where
    core = try $ do
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
      return (Nothing, M.Type (M.T_or l r a b) Nothing)

t_option fp = do
  symbol "option"
  (f, t) <- fieldType fp
  (fa, a) <- field
  return (f, M.Type (M.T_option fa a) t)

t_lambda fp = core <|> slashLambda
  where
    core = try $ do
      symbol "lambda"
      (f, t) <- fieldType fp
      a <- type_
      b <- type_
      return (f, M.Type (M.T_lambda a b) t)
    slashLambda = try $ do
      symbol "\\"
      (f, t) <- fieldType fp
      a <- type_
      symbol "->"
      b <- type_
      return (f, M.Type (M.T_lambda a b) t)

-- Container types
t_list fp = core <|> bracketList
  where
    core = try $ do
      symbol "list"
      (f, t) <- fieldType fp
      a <- type_
      return (f, M.Type (M.T_list a) t)
    bracketList = try $ do
      a <- brackets type_
      (f, t) <- fieldType fp
      return (f, M.Type (M.T_list a) t)

t_set fp = core <|> braceSet
  where
    core = try $ do
      symbol "set"
      (f, t) <- fieldType fp
      a <- comparable
      return (f, M.Type (M.T_set a) t)
    braceSet = try $ do
      a <- braces comparable
      (f, t) <- fieldType fp
      return (f, M.Type (M.T_set a) t)

t_map fp = (do symbol "map"; (f, t) <- fieldType fp; a <- comparable; b <- type_; return (f, M.Type (M.T_map a b) t))
t_big_map fp = (do symbol "big_map"; (f, t) <- fieldType fp; a <- comparable; b <- type_; return (f, M.Type (M.T_big_map a b) t))

{- Operations Parsers -}
ops :: Parser [M.ParsedOp]
ops = braces $ sepEndBy (prim' <|> mac' <|> seq') semicolon
  where
    prim' = M.PRIM <$> try prim
    mac'  = M.MAC <$> try macro
    seq'  = M.SEQ <$> try ops

prim :: Parser M.ParsedInstr
prim = choice
  [ dropOp, dupOp, swapOp, pushOp, someOp, noneOp, unitOp, ifNoneOp, pairOp
  , carOp, cdrOp, leftOp, rightOp, ifLeftOp, ifRightOp, nilOp, consOp, ifConsOp
  , sizeOp, emptySetOp, emptyMapOp, mapOp, iterOp, memOp, getOp, updateOp, ifOp
  , loopLOp, loopOp, lambdaOp, execOp, dipOp, failWithOp, castOp, renameOp
  , concatOp, packOp, unpackOp, sliceOp, isNatOp, addressOp, addOp, subOp
  , mulOp, edivOp, absOp, negOp, modOp, lslOp, lsrOp, orOp, andOp, xorOp, notOp
  , compareOp, eqOp, neqOp, ltOp, leOp, gtOp, geOp, intOp, selfOp, contractOp
  , transferTokensOp, setDelegateOp, createAccountOp, createContract2Op
  , createContractOp, implicitAccountOp, nowOp, amountOp, balanceOp, checkSigOp
  , sha256Op, sha512Op, blake2BOp, hashKeyOp, stepsToQuotaOp, sourceOp, senderOp
  ]
-------------------------------------------------------------------------------
-- Core instructions
-------------------------------------------------------------------------------

-- Control Structures
failWithOp = do symbol' "FAILWITH"; return M.FAILWITH
ifOp    = do symbol' "IF"; a <- ops; M.IF a <$> ops
loopOp  = do symbol' "LOOP"; M.LOOP <$> ops
loopLOp = do symbol' "LOOP_LEFT"; M.LOOP_LEFT <$> ops
execOp  = do symbol' "EXEC"; M.EXEC <$> noteVDef
dipOp   = do symbol' "DIP"; M.DIP <$> ops

-- Stack Operations
dropOp   = do symbol' "DROP"; return M.DROP;
dupOp    = do symbol' "DUP"; M.DUP <$> noteVDef
swapOp   = do symbol' "SWAP"; return M.SWAP;
pushOp   = do symbol' "PUSH"; v <- noteVDef; a <- type_; M.PUSH v a <$> data_
unitOp   = do symbol' "UNIT"; (t, v) <- notesTV; return $ M.UNIT t v
lambdaOp = do symbol' "LAMBDA"; v <- noteVDef; a <- type_; b <- type_;
              M.LAMBDA v a b <$> ops

-- Generic comparison
eqOp  = do symbol' "EQ"; M.EQ <$> noteVDef
neqOp = do symbol' "NEQ"; M.NEQ <$> noteVDef
ltOp  = do symbol' "LT"; M.LT <$> noteVDef
gtOp  = do symbol' "GT"; M.GT <$> noteVDef
leOp  = do symbol' "LE"; M.LE <$> noteVDef
geOp  = do symbol' "GE"; M.GE <$> noteVDef

-- ad-hoc comparison
compareOp = do symbol' "COMPARE"; M.COMPARE <$> noteVDef

-- Operations on booleans
orOp  = do symbol' "OR";  M.OR <$> noteVDef
andOp = do symbol' "AND"; M.AND <$> noteVDef
xorOp = do symbol' "XOR"; M.XOR <$> noteVDef
notOp = do symbol' "NOT"; M.NOT <$> noteVDef

-- Operations on integers and natural numbers
addOp  = do symbol' "ADD"; M.ADD <$> noteVDef
subOp  = do symbol' "SUB"; M.SUB <$> noteVDef
mulOp  = do symbol' "MUL"; M.MUL <$> noteVDef
edivOp = do symbol' "EDIV";M.EDIV <$> noteVDef
absOp  = do symbol' "ABS"; M.ABS <$> noteVDef
negOp  = do symbol' "NEG"; return M.NEG;
modOp  = do symbol' "MOD"; return M.MOD;

-- Bitwise logical operators
lslOp = do symbol' "LSL"; M.LSL <$> noteVDef
lsrOp = do symbol' "LSR"; M.LSR <$> noteVDef

-- Operations on string's
concatOp = do symbol' "CONCAT"; M.CONCAT <$> noteVDef
sliceOp  = do symbol' "SLICE"; M.SLICE <$> noteVDef

-- Operations on pairs
pairOp = do symbol' "PAIR"; (t, v, (p, q)) <- notesTVF2; return $ M.PAIR t v p q
carOp  = do symbol' "CAR"; (v, f) <- notesVF; return $ M.CAR v f
cdrOp  = do symbol' "CDR"; (v, f) <- notesVF; return $ M.CDR v f

-- Operations on collections (sets, maps, lists)
emptySetOp = do symbol' "EMPTY_SET"; (t, v) <- notesTV;
                M.EMPTY_SET t v <$> comparable
emptyMapOp = do symbol' "EMPTY_MAP"; (t, v) <- notesTV; a <- comparable;
                M.EMPTY_MAP t v a <$> type_
memOp      = do symbol' "MEM"; M.MEM <$> noteVDef
updateOp   = do symbol' "UPDATE"; return M.UPDATE
iterOp     = do symbol' "ITER"; v <- noteVDef; M.ITER v <$> ops
sizeOp     = do symbol' "SIZE"; M.SIZE <$> noteVDef
mapOp      = do symbol' "MAP"; v <- noteVDef; M.MAP v <$> ops
getOp      = do symbol' "GET"; M.GET <$> noteVDef
nilOp      = do symbol' "NIL"; (t, v) <- notesTV; M.NIL t v <$> type_
consOp     = do symbol' "CONS"; M.CONS <$> noteVDef
ifConsOp   = do symbol' "IF_CONS"; a <- ops; M.IF_CONS a <$> ops

-- Operations on options
someOp   = do symbol' "SOME"; (t, v, f) <- notesTVF; return $ M.SOME t v f
noneOp   = do symbol' "NONE"; (t, v, f) <- notesTVF; M.NONE t v f <$> type_
ifNoneOp = do symbol' "IF_NONE"; a <- ops; M.IF_NONE a <$> ops

-- Operations on unions
leftOp    = do symbol' "LEFT"; (t, v, (f, f')) <- notesTVF2;
               M.LEFT t v f f' <$> type_
rightOp   = do symbol' "RIGHT"; (t, v, (f, f')) <- notesTVF2;
               M.RIGHT t v f f' <$> type_
ifLeftOp  = do symbol' "IF_LEFT"; a <- ops; M.IF_LEFT a <$> ops
ifRightOp = do symbol' "IF_RIGHT"; a <- ops; M.IF_RIGHT a <$> ops

-- Operations on contracts
createContractOp  = do symbol' "CREATE_CONTRACT"; v <- noteVDef;
                       M.CREATE_CONTRACT v <$> noteVDef
createContract2Op = do symbol' "CREATE_CONTRACT"; v <- noteVDef; v' <- noteVDef;
                       M.CREATE_CONTRACT2 v v' <$> braces contract
createAccountOp   = do symbol' "CREATE_ACCOUNT"; v <- noteVDef; v' <- noteVDef;
                       return $ M.CREATE_ACCOUNT v v'
transferTokensOp  = do symbol' "TRANSFER_TOKENS"; M.TRANSFER_TOKENS <$> noteVDef
setDelegateOp     = do symbol' "SET_DELEGATE"; return M.SET_DELEGATE
balanceOp         = do symbol' "BALANCE"; M.BALANCE <$> noteVDef
contractOp        = do symbol' "CONTRACT"; M.CONTRACT <$> type_
sourceOp          = do symbol' "SOURCE"; M.SOURCE <$> noteVDef
senderOp          = do symbol' "SENDER"; M.SENDER <$> noteVDef
amountOp          = do symbol' "AMOUNT"; M.AMOUNT <$> noteVDef
implicitAccountOp = do symbol' "IMPLICIT_ACCOUNT"; M.IMPLICIT_ACCOUNT <$> noteVDef
selfOp            = do symbol' "SELF"; M.SELF <$> noteVDef
addressOp         = do symbol' "ADDRESS"; M.ADDRESS <$> noteVDef

-- Special Operations
nowOp          = do symbol' "NOW"; M.NOW <$> noteVDef
stepsToQuotaOp = do symbol' "STEPS_TO_QUOTA"; M.STEPS_TO_QUOTA <$> noteVDef

-- Operations on bytes
packOp   = do symbol' "PACK"; M.PACK <$> noteVDef
unpackOp = do symbol' "UNPACK"; v <- noteVDef; M.UNPACK v <$> type_

-- Cryptographic Primitives
checkSigOp = do symbol' "CHECK_SIGNATURE"; M.CHECK_SIGNATURE <$> noteVDef
blake2BOp  = do symbol' "BLAKE2B"; M.BLAKE2B <$> noteVDef
sha256Op   = do symbol' "SHA256"; M.SHA256 <$> noteVDef
sha512Op   = do symbol' "SHA512"; M.SHA512 <$> noteVDef
hashKeyOp  = do symbol' "HASH_KEY"; M.HASH_KEY <$> noteVDef

{- Type operations -}
castOp = do symbol' "CAST"; t <- type_; M.CAST t <$> noteVDef
renameOp = do symbol' "RENAME"; M.RENAME <$> noteVDef
isNatOp = do symbol' "ISNAT"; return M.ISNAT
intOp = do symbol' "INT"; M.INT <$> noteVDef

-------------------------------------------------------------------------------
-- Macros
-------------------------------------------------------------------------------
cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp <|> geOp

macro :: Parser M.Macro
macro = do symbol' "CMP"; a <- cmpOp; M.CMP a <$> noteVDef
  <|> do symbol' "IFCMP"; a <- cmpOp; v <- noteVDef; b <- ops;
         M.IFCMP a v b <$> ops
  <|> do symbol' "IF_SOME"; a <- ops; M.IF_SOME a <$> ops
  <|> do symbol' "IF"; a <- cmpOp; bt <- ops; M.IFX a bt <$> ops
  <|> do symbol' "FAIL"; return M.FAIL
  <|> do symbol' "ASSERT_CMP"; M.ASSERT_CMP <$> cmpOp
  <|> do symbol' "ASSERT_NONE"; return M.ASSERT_NONE
  <|> do symbol' "ASSERT_SOME"; return M.ASSERT_SOME
  <|> do symbol' "ASSERT_LEFT"; return M.ASSERT_LEFT
  <|> do symbol' "ASSERT_RIGHT"; return M.ASSERT_RIGHT
  <|> do symbol' "ASSERT_"; M.ASSERTX <$> cmpOp
  <|> do symbol' "ASSERT"; return M.ASSERT
  <|> do string' "DI"; n <- num "I"; symbol' "P"; M.DIIP (n + 1) <$> ops
  <|> do string' "DU"; n <- num "U"; symbol' "P"; M.DUUP (n + 1) <$> noteVDef
  <|> pairMac
  <|> unpairMac
  <|> cadrMac
  <|> setCadrMac
  <|> mapCadrMac
  where
   num str = fromIntegral . length <$> some (string' str)

pairMac :: Parser M.Macro
pairMac = do
  a <- pairMacInner
  symbol' "R"
  (tn, vn, fns) <- permute3Def noteTDef noteV (some noteF)
  let ps = Macro.mapLeaves ((Nothing,) <$> fns) a
  return $ M.PAPAIR ps tn vn

pairMacInner :: Parser M.PairStruct
pairMacInner = do
  string' "P"
  l <- (string' "A" >> return (M.F (Nothing, Nothing))) <|> pairMacInner
  r <- (string' "I" >> return (M.F (Nothing, Nothing))) <|> pairMacInner
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
