-- | Parsing of built-in Michelson macros.

module Michelson.Parser.Macro
  ( macro
  -- * These are handled separately to have better error messages
  , pairMac
  , ifCmpMac
  , mapCadrMac
  ) where

import Prelude hiding (try)

import Text.Megaparsec (notFollowedBy, try)

import Michelson.Lexer
import qualified Michelson.Macro as Macro
import Michelson.Parser.Annotations
import Michelson.Parser.Instr
import Michelson.Parser.Types (Parser)
import Michelson.Types (CadrStruct(..), Macro(..), PairStruct(..), ParsedOp(..))
import Michelson.Untyped (noAnn)
import Util.Alternative (someNE)

macro :: Parser ParsedOp -> Parser Macro
macro opParser =
      do symbol' "CASE"; is <- someNE ops; return $ CASE is
  <|> do symbol' "VIEW"; a <- ops; return $ VIEW a
  <|> do symbol' "VOID"; a <- ops; return $ VOID a
  <|> do symbol' "CMP"; a <- cmpOp; CMP a <$> noteVDef
  <|> do void $ symbol' "IF_SOME"; IF_SOME <$> ops <*> ops
  <|> do void $ symbol' "IF_RIGHT"; IF_RIGHT <$> ops <*> ops
  <|> do symbol' "FAIL"; return FAIL
  <|> do void $ symbol' "ASSERT_CMP"; ASSERT_CMP <$> cmpOp
  <|> do symbol' "ASSERT_NONE"; return ASSERT_NONE
  <|> do symbol' "ASSERT_SOME"; return ASSERT_SOME
  <|> do symbol' "ASSERT_LEFT"; return ASSERT_LEFT
  <|> do symbol' "ASSERT_RIGHT"; return ASSERT_RIGHT
  <|> do void $ symbol' "ASSERT_"; ASSERTX <$> cmpOp
  <|> do symbol' "ASSERT"; return ASSERT
  <|> do string' "DI"; n <- num "I"; symbol' "P"; DIIP (n + 1) <$> ops
  <|> do string' "DU"; n <- num "U"; symbol' "P"; DUUP (n + 1) <$> noteVDef
  <|> unpairMac
  <|> cadrMac
  <|> setCadrMac
  where
   ops = ops' opParser
   num str = fromIntegral . length <$> some (string' str)

pairMac :: Parser Macro
pairMac = do
  a <- pairMacInner
  symbol' "R"
  (tn, vn, fns) <- permute3Def noteTDef noteV (some noteF)
  let ps = Macro.mapLeaves ((noAnn,) <$> fns) a
  return $ PAPAIR ps tn vn

pairMacInner :: Parser PairStruct
pairMacInner = do
  string' "P"
  l <- (string' "A" >> return (F (noAnn, noAnn))) <|> pairMacInner
  r <- (string' "I" >> return (F (noAnn, noAnn))) <|> pairMacInner
  return $ P l r

unpairMac :: Parser Macro
unpairMac = do
  string' "UN"
  a <- pairMacInner
  symbol' "R"
  (vns, fns) <- permute2Def (some noteV) (some noteF)
  return $ UNPAIR (Macro.mapLeaves (zip vns fns) a)

cadrMac :: Parser Macro
cadrMac = lexeme $ do
  string' "C"
  a <- some $ try $ cadrInner <* notFollowedBy (string' "R")
  b <- cadrInner
  symbol' "R"
  (vn, fn) <- notesVF
  return $ CADR (a ++ pure b) vn fn

cadrInner :: Parser CadrStruct
cadrInner = (string' "A" >> return A) <|> (string' "D" >> return D)

setCadrMac :: Parser Macro
setCadrMac = do
  string' "SET_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  return $ SET_CADR a v f

mapCadrMac :: Parser ParsedOp -> Parser Macro
mapCadrMac opParser = do
  string' "MAP_C"
  a <- some cadrInner
  symbol' "R"
  (v, f) <- notesVF
  MAP_CADR a v f <$> ops' opParser

ifCmpMac :: Parser ParsedOp -> Parser Macro
ifCmpMac opParser =
  symbol' "IFCMP" *>
  (IFCMP <$> cmpOp <*> noteVDef <*> ops' opParser <*> ops' opParser)
