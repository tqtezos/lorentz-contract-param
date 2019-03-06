module Morley.Parser.Annotations
  ( note
  , noteT
  , noteV
  , noteF
  , noteF2
  , noteTDef
  , noteVDef
  , _noteFDef
  , notesTVF
  , notesTVF2
  , notesTV
  , notesVF
  , fieldType
  , permute2Def
  , permute3Def
  ) where

import Prelude hiding (many, note, some, try)

import Control.Applicative.Permutations
  (runPermutation, toPermutationWithDefault)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import qualified Data.Text as T
import Text.Megaparsec (satisfy, takeWhileP, try)
import Text.Megaparsec.Char (string)

import Morley.Default
import Morley.Lexer
import Morley.Types (Parser)
import qualified Morley.Types as M

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

noteT :: Parser M.TypeAnn
noteT = M.ann <$> note ":"

noteV :: Parser M.VarAnn
noteV = M.ann <$> note "@"

noteF :: Parser M.FieldAnn
noteF = M.ann <$> note "%"

noteF2 :: Parser (M.FieldAnn, M.FieldAnn)
noteF2 = do a <- noteF; b <- noteF; return (a, b)

parseDef :: Default a => Parser a -> Parser a
parseDef a = try a <|> pure def

noteTDef :: Parser M.TypeAnn
noteTDef = parseDef noteT

noteVDef :: Parser M.VarAnn
noteVDef = parseDef noteV

_noteFDef :: Parser M.FieldAnn
_noteFDef = parseDef noteF

notesTVF :: Parser (M.TypeAnn, M.VarAnn, M.FieldAnn)
notesTVF = permute3Def noteT noteV noteF

notesTVF2 :: Parser (M.TypeAnn, M.VarAnn, (M.FieldAnn, M.FieldAnn))
notesTVF2 = permute3Def noteT noteV noteF2

notesTV :: Parser (M.TypeAnn, M.VarAnn)
notesTV = permute2Def noteT noteV

notesVF :: Parser (M.VarAnn, M.FieldAnn)
notesVF  = permute2Def noteV noteF

fieldType :: Default a
          => Parser a
          -> Parser (a, M.TypeAnn)
fieldType fp = runPermutation $
  (,) <$> toPermutationWithDefault  def     fp
      <*> toPermutationWithDefault M.noAnn noteT
