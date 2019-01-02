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

import qualified Language.Michelson.Macro         as Macro
import qualified Language.Michelson.Types         as M

import           Data.Maybe
import           Data.Natural
import           Data.Void                        (Void)

import           Control.Applicative.Permutations


type Parser = Parsec Void T.Text

-- top-level parsers
contract :: Parser M.Contract
contract = do
  mSpace
  (p,s,c) <- runPermutation $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ M.Contract p s c

parameter = do symbol "parameter"; a <- type_; semicolon; return $ M.Parameter a
storage   = do symbol "storage"; a <- type_; semicolon; return $ M.Storage a;
code      = do symbol "code"; a <- ops; optional semicolon; return $ M.Code a


-- Lexing
lexeme = L.lexeme mSpace
mSpace = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol = L.symbol mSpace
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
semicolon = symbol ";"

{- Data Parsers -}
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

listData = braces $ sepEndBy data_ semicolon
eltData = do symbol "Elt"; key <- data_; val <- data_; return $ M.Elt key val
mapData = braces $ sepEndBy eltData semicolon

intLiteral = (L.signed (return ()) L.decimal)

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

{- Permutation Parsers -}
class Default a where def :: a

instance Default (Maybe a)                       where def = Nothing
instance Default [a]                             where def = []
instance (Default a, Default b) => Default (a,b) where def = (def, def)

permute2Def :: (Default a, Default b) => Parser a -> Parser b -> Parser (a,b)
permute2Def a b = runPermutation $
  (,) <$> (toPermutationWithDefault def a)
      <*> (toPermutationWithDefault def b)

permute3Def :: (Default a, Default b, Default c) =>
                Parser a -> Parser b -> Parser c -> Parser (a,b,c)
permute3Def a b c = runPermutation $
  (,,) <$> (toPermutationWithDefault def a)
       <*> (toPermutationWithDefault def b)
       <*> (toPermutationWithDefault def c)

-- General T/V/F Annotation parser
note :: T.Text -> Parser T.Text
note c = lexeme $ string c >> (note' <|> emptyNote)
  where
    emptyNote = pure ""
    note' = do
      a <- string "@"
           <|> string "%"
           <|> string "%%"
           <|> T.singleton <$> satisfy (\ x -> isAlpha x && isAscii x)
      let validChar x =
            (isAscii x) && (isAlphaNum x || x == '\\' || x == '.' || x == '_')
      b <- takeWhileP Nothing validChar
      return $ T.append a b

noteT :: Parser M.TypeNote
noteT = Just <$> note ":"

noteV :: Parser M.VarNote
noteV = Just <$> note "@"

noteF :: Parser M.FieldNote
noteF = Just <$> note "%"

noteF2 :: Parser (M.FieldNote, M.FieldNote)
noteF2 = do a <- noteF; b <- noteF; return (a, b)

parseDef :: Default a => Parser a -> Parser a
parseDef a = (try a) <|> pure def

noteTDef = parseDef noteT
noteVDef = parseDef noteV
noteFDef = parseDef noteF

notesTVF :: Parser (M.TypeNote, M.VarNote, M.FieldNote)
notesTVF = permute3Def noteT noteV noteF

notesTVF2 :: Parser (M.TypeNote, M.VarNote, (M.FieldNote, M.FieldNote))
notesTVF2 = permute3Def noteT noteV noteF2

notesTV :: Parser (M.TypeNote, M.VarNote)
notesTV = permute2Def noteT noteV

notesVF :: Parser (M.VarNote, M.FieldNote)
notesVF  = permute2Def noteV noteF

{- Type Parsers -}
field :: Parser M.Type
field = (try $ typeInner noteFDef) <|> (parens $ typeInner noteFDef)

typeField :: Parser M.FieldNote -> Parser (M.TypeNote, M.FieldNote)
typeField fp = permute2Def noteT fp

type_ :: Parser M.Type
type_ = lexeme $ (ti <|> parens ti)
  where
    ti = typeInner (pure Nothing)

typeInner :: Parser M.FieldNote -> Parser M.Type
typeInner fp = lexeme $ comparableType fp
  <|> (do symbol "key"; (t, f) <- typeField fp
          return $ M.Type (M.T_key) t f)
  <|> (do symbol "unit"; (t, f) <- typeField fp;
          return $ M.Type M.T_unit t f;)
  <|> (do symbol "signature"; (t, f) <- typeField fp;
          return $ M.Type M.T_signature t f)
  <|> (do symbol "option"; (t, f) <- typeField fp; a <- field;
          return $ M.Type (M.T_option a) t f)
  <|> (do symbol "list"; (t, f) <- typeField fp; a <- type_;
          return $ M.Type (M.T_list a) t f;)
  <|> (do symbol "set"; (t, f) <- typeField fp; a <- comparable;
          return $ M.Type (M.T_set a) t f)
  <|> (do symbol "operation"; (t, f) <- typeField fp;
          return $ M.Type M.T_operation t f)
  -- <|> (do symbol "address"; (t, f) <- typeAndField fp; return $ M.Type M.T_address t f)
  <|> (do symbol "contract"; (t, f) <- typeField fp; a <- type_;
          return $ M.Type (M.T_contract a) t f)
  <|> (do symbol "pair"; (t, f) <- typeField fp; a <- field; b <- field;
          return $ M.Type (M.T_pair a b) t f)
  <|> (do symbol "or"; (t, f) <- typeField fp; a <- field; b <- field;
          return $ M.Type (M.T_or a b) t f)
  <|> (do symbol "lambda"; (t, f) <- typeField fp; a <- type_; b <- type_;
          return $ M.Type (M.T_lambda a b) t f)
  <|> (do symbol "map"; (t, f) <- typeField fp; a <- comparable; b <- type_;
          return $ M.Type (M.T_map a b) t f)
  <|> (do symbol "big_map"; (t, f) <- typeField fp; a <- comparable; b <- type_;
          return $ M.Type (M.T_map a b) t f)

-- Comparable Types
comparableType :: Parser M.FieldNote -> Parser M.Type
comparableType fp =
  do ct <- ct; (t, f) <- typeField fp; return (M.Type (M.T_comparable ct) t f);

comparable :: Parser M.Comparable
comparable = let c = do ct <- ct; t <- noteTDef; return $ M.Comparable ct t
    in parens c <|> c

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

{- Operations Parsers -}
ops :: Parser [M.Op]
ops = (braces $ sepEndBy (prim' <|> mac' <|> seq') semicolon)
  where
    prim' = M.PRIM <$> (try prim)
    mac'  = M.MAC <$> (try macro)
    seq'  = M.SEQ <$> (try ops)

prim :: Parser M.I
prim = dropOp
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
  <|> ifLeftOp
  <|> ifRightOp
  <|> nilOp
  <|> consOp
  <|> ifConsOp
  <|> sizeOp
  <|> emptySetOp
  <|> emptyMapOp
  <|> mapOp
  <|> iterOp
  <|> memOp
  <|> getOp
  <|> updateOp
  <|> ifOp
  <|> loopLOp
  <|> loopOp
  <|> lambdaOp
  <|> execOp
  <|> dipOp
  <|> failWithOp
  <|> castOp
  <|> renameOp
  <|> concatOp
  <|> packOp
  <|> unpackOp
  <|> sliceOp
  <|> isNatOp
  <|> addressOp
  <|> addOp
  <|> subOp
  <|> mulOp
  <|> edivOp
  <|> absOp
  <|> negOp
  <|> modOp
  <|> lslOp
  <|> lsrOp
  <|> orOp
  <|> andOp
  <|> xorOp
  <|> notOp
  <|> compareOp
  <|> eqOp
  <|> neqOp
  <|> ltOp
  <|> leOp
  <|> gtOp
  <|> geOp
  <|> intOp
  <|> selfOp
  <|> contractOp
  <|> transferTokensOp
  <|> setDelegateOp
  <|> createAccountOp
  <|> createContract2Op
  <|> createContractOp
  <|> implicitAccountOp
  <|> nowOp
  <|> amountOp
  <|> balanceOp
  <|> checkSigOp
  <|> sha256Op
  <|> sha512Op
  <|> blake2BOp
  <|> hashKeyOp
  <|> stepsToQuotaOp
  <|> sourceOp
  <|> senderOp

{- Core instructions -}
-- Control Structures
failWithOp = do symbol "FAILWITH"; return M.FAILWITH
ifOp    = do symbol "IF"; a <- ops; b <- ops; return $ M.IF a b
loopOp  = do symbol "LOOP"; a <- ops; return $ M.LOOP a;
loopLOp = do symbol "LOOP_LEFT"; a <- ops; return $ M.LOOP_LEFT a;
execOp  = do symbol "EXEC"; v <- noteVDef; return $ M.EXEC v;
dipOp   = do symbol "DIP"; a <- ops; return $ M.DIP a

-- Stack Operations
dropOp   = do symbol "DROP"; return M.DROP;
dupOp    = do symbol "DUP"; v <- noteVDef; return $ M.DUP v;
swapOp   = do symbol "SWAP"; return M.SWAP;
pushOp   = do symbol "PUSH"; v <- noteVDef; a <- type_; b <- data_;
              return $ M.PUSH v a b;
unitOp   = do symbol "UNIT"; (t, v) <- notesTV; return $ M.UNIT t v
lambdaOp = do symbol "LAMBDA"; v <- noteVDef; a <- type_; b <- type_; c <- ops;
              return $ M.LAMBDA v a b c

-- Generic comparison
eqOp  = do symbol "EQ";  v <- noteVDef; return $ M.EQ v
neqOp = do symbol "NEQ"; v <- noteVDef; return $ M.NEQ v
ltOp  = do symbol "LT";  v <- noteVDef; return $ M.LT v
gtOp  = do symbol "GT";  v <- noteVDef; return $ M.GT v
leOp  = do symbol "LE";  v <- noteVDef; return $ M.LE v
geOp  = do symbol "GE";  v <- noteVDef; return $ M.GE v

-- ad-hoc comparison
compareOp = do symbol "COMPARE"; v <- noteVDef; return $ M.COMPARE v

{- Operations on Data -}
-- Operations on booleans
orOp  = do symbol "OR"; v <- noteVDef; return $ M.OR v;
andOp = do symbol "AND"; v <- noteVDef; return $ M.AND v
xorOp = do symbol "XOR"; v <- noteVDef; return $ M.XOR v;
notOp = do symbol "NOT"; v <- noteVDef; return $ M.NOT v;

-- Operations on integers and natural numbers
addOp  = do symbol "ADD"; v <- noteVDef; return $ M.ADD v
subOp  = do symbol "SUB"; v <- noteVDef; return $ M.SUB v
mulOp  = do symbol "MUL"; v <- noteVDef; return $ M.MUL v
edivOp = do symbol "EDIV"; v <- noteVDef; return $ M.EDIV v
absOp  = do symbol "ABS"; v <- noteVDef; return $ M.ABS v
negOp  = do symbol "NEG"; return M.NEG;
modOp  = do symbol "MOD"; return M.MOD;

-- Bitwise logical operators
lslOp = do symbol "LSL"; v <- noteVDef; return $ M.LSL v
lsrOp = do symbol "LSR"; v <- noteVDef; return $ M.LSR v

-- Operations on strings
concatOp = do symbol "CONCAT"; v <- noteVDef; return $ M.CONCAT v
sliceOp  = do symbol "SLICE"; v <- noteVDef; return $ M.SLICE v;

-- Operations on pairs
pairOp = do symbol "PAIR"; (t, v, (p, q)) <- notesTVF2; return $ M.PAIR t v p q
carOp  = do symbol "CAR"; (v, f) <- notesVF; return $ M.CAR v f
cdrOp  = do symbol "CDR"; (v, f) <- notesVF; return $ M.CDR v f

-- Operations on collections (sets, maps, lists)
emptySetOp = do symbol "EMPTY_SET"; (t, v) <- notesTV; a <- comparable;
                return $ M.EMPTY_SET t v a
emptyMapOp = do symbol "EMPTY_MAP";
                (t, v) <- notesTV; a <- comparable; b <- type_;
                return $ M.EMPTY_MAP t v a b
memOp      = do symbol "MEM"; v <- noteVDef; return $ M.MEM v
updateOp   = do symbol "UPDATE"; return M.UPDATE
iterOp     = do symbol "ITER"; v <- noteVDef; a <- ops; return $ M.ITER v a
sizeOp     = do symbol "SIZE"; a <- noteVDef; return $ M.SIZE a
mapOp      = do symbol "MAP"; v <- noteVDef; a <- ops; return $ M.MAP v a
getOp      = do symbol "GET"; v <- noteVDef; return $ M.GET v
nilOp      = do symbol "NIL"; (t, v) <- notesTV; a <- type_;
                return $ M.NIL t v a
consOp     = do symbol "CONS"; v <- noteVDef; return $ M.CONS v;
ifConsOp   = do symbol "IF_CONS"; a <- ops; b <- ops; return $ M.IF_CONS a b

-- Operations on options
someOp   = do symbol "SOME"; (t, v, f) <- notesTVF; return $ M.SOME t v f
noneOp   = do symbol "NONE"; (t, v, f) <- notesTVF; a <- type_;
              return $ M.NONE t v f a;
ifNoneOp = do symbol "IF_NONE"; a <- ops; b <- ops; return $ M.IF_NONE a b

-- Operations on unions
leftOp    = do symbol "LEFT"; (t, v, (f, f')) <- notesTVF2; a <- type_;
               return $ M.LEFT t v f f' a
rightOp   = do symbol "RIGHT"; (t, v, (f, f')) <- notesTVF2; a <- type_;
               return $ M.RIGHT t v f f' a
ifLeftOp  = do symbol "IF_LEFT"; a <- ops; b <- ops; return $ M.IF_LEFT a b
ifRightOp = do symbol "IF_RIGHT"; a <- ops; b <- ops; return $ M.IF_RIGHT a b

-- Operations on contracts
createContractOp  = do symbol "CREATE_CONTRACT"; v <- noteVDef; v' <- noteVDef;
                       return $ M.CREATE_CONTRACT v v'
createContract2Op = do symbol "CREATE_CONTRACT";
                       v <- noteVDef; v' <- noteVDef; a <- braces contract;
                       return $ M.CREATE_CONTRACT2 v v' a
createAccountOp   = do symbol "CREATE_ACCOUNT"; v <- noteVDef; v' <- noteVDef;
                       return $ M.CREATE_ACCOUNT v v'
transferTokensOp  = do symbol "TRANSFER_TOKENS"; v <- noteVDef;
                       return $ M.TRANSFER_TOKENS v
setDelegateOp     = do symbol "SET_DELEGATE"; return $ M.SET_DELEGATE
balanceOp         = do symbol "BALANCE"; v <- noteVDef; return $ M.BALANCE v
contractOp        = do symbol "CONTRACT"; t <- type_; return $ M.CONTRACT t
sourceOp          = do symbol "SOURCE"; v <- noteVDef; return $ M.SOURCE v
senderOp          = do symbol "SENDER"; v <- noteVDef; return $ M.SENDER v
amountOp          = do symbol "AMOUNT"; v <- noteVDef; return $ M.AMOUNT v
implicitAccountOp = do symbol "IMPLICIT_ACCOUNT"; v <- noteVDef;
                       return $ M.IMPLICIT_ACCOUNT v
selfOp            = do symbol "SELF"; v <- noteVDef; return $ M.SELF v
addressOp         = do symbol "ADDRESS"; v <- noteVDef; return $ M.ADDRESS v

-- Special Operations
nowOp          = do symbol "NOW"; v <- noteVDef; return $ M.NOW v
stepsToQuotaOp = do symbol "STEPS_TO_QUOTA"; v <- noteVDef;
                    return $ M.STEPS_TO_QUOTA v

-- Operations on bytes
packOp   = do symbol "PACK"; v <- noteVDef; return $ M.PACK v
unpackOp = do symbol "UNPACK"; v <- noteVDef; t <- type_; return $ M.UNPACK v t

-- Cryptographic Primitives
checkSigOp = do symbol "CHECK_SIGNATURE"; v <- noteVDef;
                return $ M.CHECK_SIGNATURE v
blake2BOp  = do symbol "BLAKE2B"; v <- noteVDef; return $ M.BLAKE2B v
sha256Op   = do symbol "SHA256"; v <- noteVDef; return $ M.SHA256 v
sha512Op   = do symbol "SHA512"; v <- noteVDef; return $ M.SHA512 v
hashKeyOp  = do symbol "HASH_KEY"; v <- noteVDef; return $ M.HASH_KEY v

{- Type operations -}
castOp = do symbol "CAST"; t <- noteTDef; v <- noteVDef; return $ M.CAST t v
renameOp = do symbol "RENAME"; v <- noteVDef; return $ M.RENAME v
isNatOp = do symbol "ISNAT"; return $ M.ISNAT
intOp = do symbol "INT"; v <- noteVDef; return $ M.INT v

-- Macros
cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp <|> geOp

macro :: Parser M.Macro
macro = (do string "CMP"; a <- cmpOp; v <- noteVDef; return $ M.CMP a v)
  <|> (do string "IFCMP"; a <- cmpOp; v <- noteVDef; bt <- ops; bf <- ops;
          return $ M.IFCMP a v bt bf)
  <|> (do symbol "IF_SOME"; a <- ops; b <- ops; return $ M.IF_SOME a b)
  <|> (do string "IF"; a <- cmpOp; bt <- ops; bf <- ops; return $ M.IFX a bt bf)
  <|> (do symbol "FAIL"; return $ M.FAIL)
  <|> (do string "ASSERT_CMP"; a <- cmpOp; return $ M.ASSERT_CMP a)
  <|> (do symbol "ASSERT_NONE"; return $ M.ASSERT_NONE)
  <|> (do symbol "ASSERT_SOME"; return $ M.ASSERT_SOME)
  <|> (do symbol "ASSERT_LEFT"; return $ M.ASSERT_LEFT)
  <|> (do symbol "ASSERT_RIGHT"; return $ M.ASSERT_RIGHT)
  <|> (do string "ASSERT_"; a <- cmpOp; return $ M.ASSERTX a)
  <|> (do symbol "ASSERT"; return $ M.ASSERT)
  <|> (do string "DI"; n <- num "I"; symbol "P"; a <- ops;
          return $ M.DIIP (n + 1) a)
  <|> (do string "DU"; n <- num "U"; symbol "P"; v <- noteVDef ;
          return $ M.DUUP (n + 1) v)
  <|> pairMac
  <|> unpairMac
  <|> cadrMac
  <|> setCadrMac
  <|> mapCadrMac
  where
   num str = fromIntegral . length <$> (some $ string str);

pairMac :: Parser M.Macro
pairMac = do
  a <- pairMacInner
  symbol "R"
  (tn, vn, fns) <- permute3Def noteTDef noteV (some noteF)
  return $ M.PAPAIR (Macro.mapLeaves ((\ x -> (Nothing, x)) <$> fns) a) tn vn

pairMacInner :: Parser M.PairStruct
pairMacInner = do
  string "P"
  l <- (string "A" >> return (M.F (Nothing, Nothing))) <|> pairMacInner
  r <- (string "I" >> return (M.F (Nothing, Nothing))) <|> pairMacInner
  return $ M.P l r

unpairMac :: Parser M.Macro
unpairMac = do
  string "UN"
  a <- pairMacInner
  symbol "R"
  (vns, fns) <- permute2Def (some noteV) (some noteF)
  return $ M.UNPAIR (Macro.mapLeaves (zip vns fns) a)

cadrMac :: Parser M.Macro
cadrMac = lexeme $ do
  string "C"
  a <- some $ try $ cadrInner <* (notFollowedBy $ string "R")
  b <- cadrInner
  symbol "R"
  (vn, fn) <- notesVF
  return $ M.CADR (a ++ (pure b)) vn fn

cadrInner = (string "A" >> return M.A) <|> (string "D" >> return M.D)

setCadrMac :: Parser M.Macro
setCadrMac = do
  string "SET_C"
  a <- some cadrInner
  symbol "R"
  (v, f) <- notesVF
  return $ M.SET_CADR a v f

mapCadrMac :: Parser M.Macro
mapCadrMac = do
  string "MAP_C"
  a <- some cadrInner
  symbol "R"
  (v, f) <- notesVF
  c <- ops
  return $ M.MAP_CADR a v f c
