module Michelson.Parser.Annotations
  ( noteV
  , noteF
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
import qualified Michelson.Types as Mi
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

noteT :: Parser Mi.TypeAnn
noteT = Mi.ann <$> note ":"

noteV :: Parser Mi.VarAnn
noteV = Mi.ann <$> note "@"

noteF :: Parser Mi.FieldAnn
noteF = Mi.ann <$> note "%"

noteF2 :: Parser (Mi.FieldAnn, Mi.FieldAnn)
noteF2 = do a <- noteF; b <- noteF; return (a, b)

noteTDef :: Parser Mi.TypeAnn
noteTDef = parseDef noteT

noteVDef :: Parser Mi.VarAnn
noteVDef = parseDef noteV

notesTVF :: Parser (Mi.TypeAnn, Mi.VarAnn, Mi.FieldAnn)
notesTVF = permute3Def noteT noteV noteF

notesTVF2 :: Parser (Mi.TypeAnn, Mi.VarAnn, (Mi.FieldAnn, Mi.FieldAnn))
notesTVF2 = permute3Def noteT noteV noteF2

notesTV :: Parser (Mi.TypeAnn, Mi.VarAnn)
notesTV = permute2Def noteT noteV

notesVF :: Parser (Mi.VarAnn, Mi.FieldAnn)
notesVF  = permute2Def noteV noteF

fieldType :: Default a
          => Parser a
          -> Parser (a, Mi.TypeAnn)
fieldType fp = runPermutation $
  (,) <$> toPermutationWithDefault  def     fp
      <*> toPermutationWithDefault Mi.noAnn noteT
