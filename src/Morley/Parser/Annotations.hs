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
import qualified Morley.Types as Mo

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

noteT :: Parser Mo.TypeAnn
noteT = Mo.ann <$> note ":"

noteV :: Parser Mo.VarAnn
noteV = Mo.ann <$> note "@"

noteF :: Parser Mo.FieldAnn
noteF = Mo.ann <$> note "%"

noteF2 :: Parser (Mo.FieldAnn, Mo.FieldAnn)
noteF2 = do a <- noteF; b <- noteF; return (a, b)

parseDef :: Default a => Parser a -> Parser a
parseDef a = try a <|> pure def

noteTDef :: Parser Mo.TypeAnn
noteTDef = parseDef noteT

noteVDef :: Parser Mo.VarAnn
noteVDef = parseDef noteV

_noteFDef :: Parser Mo.FieldAnn
_noteFDef = parseDef noteF

notesTVF :: Parser (Mo.TypeAnn, Mo.VarAnn, Mo.FieldAnn)
notesTVF = permute3Def noteT noteV noteF

notesTVF2 :: Parser (Mo.TypeAnn, Mo.VarAnn, (Mo.FieldAnn, Mo.FieldAnn))
notesTVF2 = permute3Def noteT noteV noteF2

notesTV :: Parser (Mo.TypeAnn, Mo.VarAnn)
notesTV = permute2Def noteT noteV

notesVF :: Parser (Mo.VarAnn, Mo.FieldAnn)
notesVF  = permute2Def noteV noteF

fieldType :: Default a
          => Parser a
          -> Parser (a, Mo.TypeAnn)
fieldType fp = runPermutation $
  (,) <$> toPermutationWithDefault  def     fp
      <*> toPermutationWithDefault Mo.noAnn noteT
