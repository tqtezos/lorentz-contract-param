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
  | ValueSeq     [Value op]
  -- ^ A sequence of elements: can be a list or a set.
  -- We can't distinguish lists and sets during parsing.
  | ValueMap     [Elt op]
  | ValueLambda  [op]
  deriving stock (Eq, Show, Functor, Data, Generic)

data Elt op = Elt (Value op) (Value op)
  deriving stock (Eq, Show, Functor, Data, Generic)

-- | ByteString does not have an instance for ToJSON and FromJSON, to
-- avoid orphan type class instances, make a new type wrapper around it.
newtype InternalByteString = InternalByteString ByteString
  deriving stock (Data, Eq, Show)

unInternalByteString :: InternalByteString -> ByteString
unInternalByteString (InternalByteString bs) = bs

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
