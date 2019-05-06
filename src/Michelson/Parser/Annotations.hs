module Michelson.Parser.Annotations
  ( noteV
  , noteF
  , noteFDef
  , noteTDef
  , noteVDef
  , notesTVF
  , notesTVF2
  , notesTV
  , notesVF
  , fieldType
  , permute2Def
  , permute3Def
  ) where

import Prelude hiding (note)

import Control.Applicative.Permutations (runPermutation, toPermutationWithDefault)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import qualified Data.Text as T
import Text.Megaparsec (satisfy, takeWhileP)
import Text.Megaparsec.Char (string)

import Michelson.Parser.Helpers (parseDef)
import Michelson.Parser.Lexer
import Michelson.Parser.Types (Parser)
import Michelson.Untyped as U
import Util.Default

-- General T/V/F Annotation parser
note :: T.Text -> Parser T.Text
note c = lexeme $ string c >> (note' <|> emptyNote)
  where
    emptyNote = pure ""
    note' = do
      a <- string "@"
           <|> string "%%"
           <|> string "%"
           <|> T.singleton <$> satisfy (\ x -> isAlpha x && isAscii x)
      let validChar x =
            isAscii x && (isAlphaNum x || x == '\\' || x == '.' || x == '_')
      b <- takeWhileP Nothing validChar
      return $ T.append a b

noteT :: Parser U.TypeAnn
noteT = U.ann <$> note ":"

noteV :: Parser U.VarAnn
noteV = U.ann <$> note "@"

noteF :: Parser U.FieldAnn
noteF = U.ann <$> note "%"

noteFDef :: Parser U.FieldAnn
noteFDef = parseDef noteF

noteF2 :: Parser (U.FieldAnn, U.FieldAnn)
noteF2 = do a <- noteF; b <- noteF; return (a, b)

noteTDef :: Parser U.TypeAnn
noteTDef = parseDef noteT

noteVDef :: Parser U.VarAnn
noteVDef = parseDef noteV

notesTVF :: Parser (U.TypeAnn, U.VarAnn, U.FieldAnn)
notesTVF = permute3Def noteT noteV noteF

notesTVF2 :: Parser (U.TypeAnn, U.VarAnn, (U.FieldAnn, U.FieldAnn))
notesTVF2 = permute3Def noteT noteV noteF2

notesTV :: Parser (U.TypeAnn, U.VarAnn)
notesTV = permute2Def noteT noteV

notesVF :: Parser (U.VarAnn, U.FieldAnn)
notesVF  = permute2Def noteV noteF

fieldType :: Default a
          => Parser a
          -> Parser (a, U.TypeAnn)
fieldType fp = runPermutation $
  (,) <$> toPermutationWithDefault  def     fp
      <*> toPermutationWithDefault U.noAnn noteT
