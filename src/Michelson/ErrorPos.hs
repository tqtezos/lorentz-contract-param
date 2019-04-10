{-# LANGUAGE DeriveDataTypeable #-}

module Michelson.ErrorPos
  ( mkPos
  , Pos (..)
  , SrcPos (..)
  , srcPos
  , InstrCallStack (..)
  , LetCallStack
  , LetName (..)
  ) where

import Data.Data (Data(..))
import qualified Data.Text as T
import Data.Default (Default (..))
import qualified Data.Aeson as Aeson

newtype Pos = Pos Word
  deriving (Eq, Ord, Show, Generic, Data)

mkPos :: Int -> Pos
mkPos x
  | x < 0     = error $ "negative pos: " <> show x
  | otherwise = Pos $ fromIntegral x

data SrcPos = SrcPos Pos Pos
  deriving (Eq, Ord, Show, Generic, Data)

srcPos :: Word -> Word -> SrcPos
srcPos x y = SrcPos (Pos x) (Pos y)

newtype LetName = LetName T.Text
  deriving (Eq, Ord, Show, Data, Generic)

type LetCallStack = [LetName]
data InstrCallStack = InstrCallStack
  { icsCallStack :: LetCallStack
  , icsSrcPos    :: SrcPos
  } deriving (Eq, Ord, Show, Generic, Data)

instance Default Pos where
  def = Pos 0

instance Default SrcPos where
  def = SrcPos def def

instance Default InstrCallStack where
  def = InstrCallStack def def

instance Aeson.ToJSON  Pos
instance Aeson.FromJSON Pos
instance Aeson.ToJSON SrcPos
instance Aeson.FromJSON SrcPos
instance Aeson.ToJSON LetName
instance Aeson.FromJSON LetName
instance Aeson.ToJSON InstrCallStack
instance Aeson.FromJSON InstrCallStack
