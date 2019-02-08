{-# OPTIONS_GHC -Wno-missing-signatures #-}
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
import Data.Char
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char

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
           <|> string "%"
           <|> string "%%"
           <|> T.singleton <$> satisfy (\ x -> isAlpha x && isAscii x)
      let validChar x =
            isAscii x && (isAlphaNum x || x == '\\' || x == '.' || x == '_')
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
parseDef a = try a <|> pure def

noteTDef = parseDef noteT
noteVDef = parseDef noteV
_noteFDef = parseDef noteF

notesTVF :: Parser (M.TypeNote, M.VarNote, M.FieldNote)
notesTVF = permute3Def noteT noteV noteF

notesTVF2 :: Parser (M.TypeNote, M.VarNote, (M.FieldNote, M.FieldNote))
notesTVF2 = permute3Def noteT noteV noteF2

notesTV :: Parser (M.TypeNote, M.VarNote)
notesTV = permute2Def noteT noteV

notesVF :: Parser (M.VarNote, M.FieldNote)
notesVF  = permute2Def noteV noteF

fieldType fp = runPermutation $
  (,) <$> toPermutationWithDefault  def     fp
      <*> toPermutationWithDefault Nothing noteT
