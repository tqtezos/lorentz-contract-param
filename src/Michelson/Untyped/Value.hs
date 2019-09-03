{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Untyped Michelson values (i. e. type of a value is not statically known).

module Michelson.Untyped.Value
  ( Value' (..)
  , Elt (..)
  -- Internal types to avoid orphan instances
  , InternalByteString(..)
  , unInternalByteString
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Formatting.Buildable (Buildable(build))
import Text.Hex (decodeHex, encodeHex)
import Text.PrettyPrint.Leijen.Text (braces, dquotes, parens, semi, text, textStrict, (<+>))

import Michelson.Printer.Util (RenderDoc(..), buildRenderDoc, renderOps)
import Michelson.Text

data Value' op =
    ValueInt     Integer
  | ValueString  MText
  | ValueBytes   InternalByteString
  | ValueUnit
  | ValueTrue
  | ValueFalse
  | ValuePair    (Value' op) (Value' op)
  | ValueLeft    (Value' op)
  | ValueRight   (Value' op)
  | ValueSome    (Value' op)
  | ValueNone
  | ValueNil
  | ValueSeq     (NonEmpty $ Value' op)
  -- ^ A sequence of elements: can be a list or a set.
  -- We can't distinguish lists and sets during parsing.
  | ValueMap     (NonEmpty $ Elt op)
  | ValueLambda  (NonEmpty op)
  deriving stock (Eq, Show, Functor, Data, Generic)

data Elt op = Elt (Value' op) (Value' op)
  deriving stock (Eq, Show, Functor, Data, Generic)

-- | ByteString does not have an instance for ToJSON and FromJSON, to
-- avoid orphan type class instances, make a new type wrapper around it.
newtype InternalByteString = InternalByteString ByteString
  deriving stock (Data, Eq, Show)

unInternalByteString :: InternalByteString -> ByteString
unInternalByteString (InternalByteString bs) = bs

-- | `Some` was being output with redundant parens that were rejected by the
-- @tezos-client@. Below is an attempt to elide parenthases until the last
-- possible moment, stripping them when unneeded by the wrapping constructor.
instance RenderDoc op => RenderDoc (Value' op) where
  renderDoc =
    uncurry (\hasParens doc' -> bool id parens hasParens doc') . renderParensDoc
    where
      renderParensDoc = \case
        ValueNil       -> (False,) $ "{ }"
        ValueInt x     -> (False,) $ text . show $ x
        ValueString x  -> (False,) $ dquotes (textStrict $ writeMText x)
        ValueBytes xs  -> (False,) $ "0x" <> (textStrict . encodeHex . unInternalByteString $ xs)
        ValueUnit      -> (False,) $ "Unit"
        ValueTrue      -> (False,) $ "True"
        ValueFalse     -> (False,) $ "False"
        ValuePair l r  -> (True,)  $ ("Pair"  <+> renderDoc l <+> renderDoc r)
        ValueLeft l    -> (True,)  $ ("Left"  <+> renderDoc l)
        ValueRight r   -> (True,)  $ ("Right" <+> renderDoc r)
        ValueSome x    -> (True,)  $ ("Some"  <+> renderDoc x)
        ValueNone      -> (False,) $ "None"
        ValueSeq xs    -> (False,) $ braces $ mconcat $ (intersperse semi (snd . renderParensDoc <$> toList xs))
        ValueMap xs    -> (False,) $ braces $ mconcat $ (intersperse semi (renderDoc <$> toList xs))
        ValueLambda xs -> (False,) $ renderOps True xs

instance RenderDoc op => RenderDoc (Elt op) where
  renderDoc (Elt k v) = "Elt" <+> renderDoc k <+> renderDoc v

instance (RenderDoc op) => Buildable (Value' op) where
  build = buildRenderDoc

instance (RenderDoc op) => Buildable (Elt op) where
  build = buildRenderDoc

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

-- it is not possible to derives these automatically because
-- ByteString does not have a ToJSON or FromJSON instance

instance ToJSON InternalByteString where
  toJSON = toJSON . encodeHex . unInternalByteString

instance FromJSON InternalByteString where
  parseJSON =
    withText "Hex-encoded bytestring" $ \t ->
      case decodeHex t of
        Nothing -> fail "Invalid hex encoding"
        Just res -> pure (InternalByteString res)

deriveJSON defaultOptions ''Value'
deriveJSON defaultOptions ''Elt
