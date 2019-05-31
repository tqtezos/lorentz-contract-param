{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- deriving 'Container' automatically produces extra constraints.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Strings compliant with Michelson constraints.
--
-- When writting a Michelson contract, you can only mention characters with
-- codes from @[32 .. 126]@ range in string literals. Same restriction applies
-- to string literals passed to @alphanet.sh@.
--
-- However, Michelson allows some control sequences: @"\n"@. You have to write
-- it exactly in this form, and internally it will be transformed to line feed
-- character (this behaviour can be observed when looking at @Pack@ed data).
--
-- See tests for examples of good and bad strings.
module Michelson.Text
  ( MText (..)
  , mkMText
  , mkMTextUnsafe
  , mkMTextCut
  , writeMText
  , takeMText
  , dropMText
  , isMChar

    -- * Misc
  , qqMText
  , mt
  , DoNotUseTextError
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Data (Data)
import qualified Data.Text as T
import Fmt (Buildable)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import qualified Language.Haskell.TH.Quote as TH
import Test.QuickCheck (Arbitrary(..), choose, listOf)

-- | Michelson string value.
--
-- This is basically a mere text with limits imposed by the language:
-- <http://tezos.gitlab.io/zeronet/whitedoc/michelson.html#constants>
-- Although, this document seems to be not fully correct, and thus we applied
-- constraints deduced empirically.
--
-- You construct an item of this type using one of the following ways:
--
-- * With QuasyQuotes when need to create a string literal.
--
-- >>> [mt|Some text|]
-- MTextUnsafe { unMText = "Some text" }
--
-- * With 'mkMText' when constructing from a runtime text value.
--
-- * With 'mkMTextUnsafe' or 'MTextUnsafe' when absolutelly sure that
-- given string does not violate invariants.
--
-- * With 'mkMTextCut' when not sure about text contents and want
-- to make it compliant with Michelson constraints.
newtype MText = MTextUnsafe { unMText :: Text }
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (Semigroup, Monoid, Container, Buildable)

-- | Constraint on literals appearing in Michelson contract code.
isMChar :: Char -> Bool
isMChar c = fromEnum c >= 32 && fromEnum c <= 126

-- | Error message indicating bad character in a string literal.
invalidMCharError :: Char -> Text
invalidMCharError c = "Invalid character in string literal: " <> toText [c]

-- | Wrap a Haskell text into 'MText', performing necessary checks.
--
-- You can use e.g. @'\n'@ character directly in supplied argument,
-- but attempt to use other bad characters like @'\r'@ will cause failure.
mkMText :: Text -> Either Text MText
mkMText txt = mapM checkMChar (toString txt) $> MTextUnsafe txt
  where
    checkMChar c
      | isMChar c || c == '\n' = pass
      | otherwise = Left $ invalidMCharError c

-- | Contruct 'MText' from a Haskell text, failing if provided Haskell text
-- is invalid Michelson string.
mkMTextUnsafe :: HasCallStack => Text -> MText
mkMTextUnsafe = either error id . mkMText

-- | Construct 'MText' from a Haskell text, eliminating all characters which
-- should not appear in Michelson strings.
-- Characters which can be displayed normally via escaping are preserved.
mkMTextCut :: Text -> MText
mkMTextCut txt =
  MTextUnsafe . toText . filter isAllowed $ toString txt
  where
    isAllowed c = isMChar c || c == '\n'

-- | Print 'MText' for Michelson code, with all unusual characters escaped.
writeMText :: MText -> Text
writeMText (MTextUnsafe t) = t
  & T.replace "\\" "\\\\"
  & T.replace "\n" "\\n"
  & T.replace "\"" "\\\""

takeMText :: Int -> MText -> MText
takeMText n (MTextUnsafe txt) = MTextUnsafe $ T.take n txt

dropMText :: Int -> MText -> MText
dropMText n (MTextUnsafe txt) = MTextUnsafe $ T.drop n txt

instance ToText MText where
  toText = unMText

instance Arbitrary MText where
  arbitrary =
    mkMTextUnsafe . toText <$>
    listOf (choose @Char (toEnum 32, toEnum 126))

instance ToJSON MText where
  toJSON = toJSON . unMText
instance FromJSON MText where
  parseJSON v =
    either (fail . toString) pure . mkMText =<< parseJSON @Text v

-- | QuasyQuoter for constructing Michelson strings.
--
-- Validity of result will be checked at compile time.
-- Note:
--
-- * slash must be escaped
-- * newline character must appear as '\n'
-- * use quotes as is
-- * other special characters are not allowed.

-- TODO: maybe enforce one space in the beginning and one in the end?
-- compare:
-- >>> [mt|mystuff|]
-- vs
-- >>> [mt| mystuff |]
mt :: TH.QuasiQuoter
mt = TH.QuasiQuoter
  { TH.quoteExp = \s ->
      case qqMText s of
        Left err -> fail $ toString err
        Right txt -> [e| MTextUnsafe (toText @String txt) |]
  , TH.quotePat = \_ ->
      fail "Cannot use this QuasyQuotation at pattern position"
  , TH.quoteType = \_ ->
      fail "Cannot use this QuasyQuotation at type position"
  , TH.quoteDec = \_ ->
      fail "Cannot use this QuasyQuotation at declaration position"
  }

{-# ANN module ("HLint: ignore Use list literal pattern" :: Text) #-}

-- | Parser used in 'mt' quasi quoter.
qqMText :: String -> Either Text String
qqMText txt = scan txt
  where
  scan = \case
    '\\' : [] -> Left "Unterminated '\' in string literal"
    '\\' : '\\' : s -> ('\\' :) <$> scan s
    '\\' : 'n'  : s -> ('\n' :) <$> scan s
    '\\' : c : _ -> Left $ "Unknown escape sequence: '\\" <> toText [c] <> "'"
    c : s
      | isMChar c -> (c :) <$> scan s
      | otherwise -> Left $ invalidMCharError c
    [] -> Right []

instance
    TypeError ('Text "There is no instance defined for (IsString MText)" ':$$:
               'Text "Consider using QuasiQuotes: `[mt|some text...|]`"
              ) =>
    IsString MText where
  fromString = error "impossible"

-- | A type error asking to use 'MText' instead of 'Text'.
type family DoNotUseTextError where
  DoNotUseTextError = TypeError
    ( 'Text "`Text` is not isomorphic to Michelson strings," ':$$:
      'Text "consider using `MText` type instead"
    )
