{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Untyped Michelson values (i. e. type of a value is not statically known).

module Michelson.Untyped.Value
  ( Value (..)
  , Elt (..)

  -- Internal types to avoid orphan instances
  , InternalByteString(..)
  , unInternalByteString
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Text.Lazy.Builder (Builder)
import Fmt (hexF, (+|), (|+))
import Formatting.Buildable (Buildable)
import qualified Formatting.Buildable as Buildable

data Value op =
    ValueInt     Integer
  | ValueString  Text
  | ValueBytes   InternalByteString
  | ValueUnit
  | ValueTrue
  | ValueFalse
  | ValuePair    (Value op) (Value op)
  | ValueLeft    (Value op)
  | ValueRight   (Value op)
  | ValueSome    (Value op)
  | ValueNone
  | ValueNil
  | ValueSeq     (NonEmpty $ Value op)
  -- ^ A sequence of elements: can be a list or a set.
  -- We can't distinguish lists and sets during parsing.
  | ValueMap     (NonEmpty $ Elt op)
  | ValueLambda  (NonEmpty op)
  deriving stock (Eq, Show, Functor, Data, Generic)

data Elt op = Elt (Value op) (Value op)
  deriving stock (Eq, Show, Functor, Data, Generic)

-- | ByteString does not have an instance for ToJSON and FromJSON, to
-- avoid orphan type class instances, make a new type wrapper around it.
newtype InternalByteString = InternalByteString ByteString
  deriving stock (Data, Eq, Show)

unInternalByteString :: InternalByteString -> ByteString
unInternalByteString (InternalByteString bs) = bs

instance Buildable op => Buildable (Value op) where
  build =
    \case
      ValueInt i -> Buildable.build i
      ValueString s -> "\"" +| s |+ "\""
      ValueBytes (InternalByteString b) -> "0x" <> hexF b
      ValueUnit -> "Unit"
      ValueTrue -> "True"
      ValueFalse -> "False"
      ValuePair a b -> "(Pair " +| a |+ " " +| b |+ ")"
      ValueLeft v -> "(Left " +| v |+ ")"
      ValueRight v -> "(Right " +| v |+ ")"
      ValueSome v -> "(Some " +| v |+ ")"
      ValueNone -> "None"
      ValueNil -> "{}"
      ValueSeq vs -> buildList vs
      ValueMap els -> buildList els
      ValueLambda ops -> buildList ops
    where
      buildList :: Buildable a => NonEmpty a -> Builder
      buildList (toList -> items) =
        "{" <>
        mconcat (intersperse "; " $ map Buildable.build items) <>
        "}"

instance Buildable op => Buildable (Elt op) where
  build (Elt a b) = "Elt " +| a |+ " " +| b |+ ""

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

-- it is not possible to derives these automatically because
-- ByteString does not have a ToJSON or FromJSON instance

instance ToJSON InternalByteString where
  toJSON = toJSON @Text . decodeUtf8 . unInternalByteString

instance FromJSON InternalByteString where
  parseJSON = fmap (InternalByteString . encodeUtf8 @Text) . parseJSON

deriveJSON defaultOptions ''Value
deriveJSON defaultOptions ''Elt
