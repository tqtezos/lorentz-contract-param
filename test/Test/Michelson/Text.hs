-- | Tests on 'MText'.
module Test.Michelson.Text
  ( test_Roundtrip
  , unit_mkMText
  , unit_parse
  , unit_mkMTextCut
  , unit_QuasiQuoter
  , unit_mt
  ) where

import Test.HUnit (Assertion, (@?), (@?=))
import Test.Tasty (TestTree)
import Fmt (pretty)
import qualified Text.Megaparsec as P

import Michelson.Parser
import qualified Michelson.Untyped as U
import Michelson.Text

import Test.Util.QuickCheck (roundtripTest)

-- | Parse string literal content to 'MText'.
parseMTextTest :: Text -> Either () MText
parseMTextTest t =
  first (const ()) $
  expectString <$> parseNoEnv (stringLiteral <* P.eof) "" ("\"" <> t <> "\"")
  where
    expectString = \case
      U.ValueString t' -> t'
      o -> error $ "Expected string, but got " <> pretty o

-- | 'MText' rountrip conversions.
test_Roundtrip :: [TestTree]
test_Roundtrip =
  [ roundtripTest writeMText parseMTextTest ]

-- | Check value against the given predicate.
(@??) :: (Show a, HasCallStack) => a -> (a -> Bool) -> Assertion
(@??) val predicate =
  predicate val @?
  ("Predicate does not hold for value " <> show val)

unit_mkMText :: Assertion
unit_mkMText = do
  mkMText ""
    @?= Right (MTextUnsafe "")
  mkMText "a ba"
    @?= Right (MTextUnsafe "a ba")
  mkMText "a\nb"
    @?= Right (MTextUnsafe "a\nb")
  mkMText "a\\nb"
    @?= Right (MTextUnsafe "a\\nb")
  mkMText "\\\\"
    @?= Right (MTextUnsafe "\\\\")
  mkMText "\""
    @?= Right (MTextUnsafe "\"")
  mkMText "\r"
    @?? isLeft
  mkMText "\t"
    @?? isLeft
  mkMText (toText @String [toEnum 5])
    @?? isLeft
  mkMText (toText @String [toEnum 127])
    @?? isLeft
  mkMText (toText @String [toEnum 300])
    @?? isLeft

unit_parse :: Assertion
unit_parse = do
  parseMTextTest ""
    @?= Right (MTextUnsafe "")
  parseMTextTest "a ba"
    @?= Right (MTextUnsafe "a ba")
  parseMTextTest "a\nb"
    @?? isLeft
  parseMTextTest "a\\nb"
    @?= Right (MTextUnsafe "a\nb")
  parseMTextTest "\\"
    @?? isLeft
  parseMTextTest "\\\\"
    @?= Right (MTextUnsafe "\\")
  parseMTextTest "\""
    @?? isLeft
  parseMTextTest "\\\""
    @?= Right (MTextUnsafe "\"")
  parseMTextTest "\r"
    @?? isLeft
  parseMTextTest "\t"
    @?? isLeft
  parseMTextTest (toText @String [toEnum 5])
    @?? isLeft
  parseMTextTest (toText @String [toEnum 127])
    @?? isLeft
  parseMTextTest (toText @String [toEnum 300])
    @?? isLeft

unit_mkMTextCut :: Assertion
unit_mkMTextCut = do
  mkMTextCut ""
    @?= MTextUnsafe ""
  mkMTextCut "a ba"
    @?= MTextUnsafe "a ba"
  mkMTextCut "a\nb"
    @?= MTextUnsafe "a\nb"
  mkMTextCut "a\\nb"
    @?= MTextUnsafe "a\\nb"
  mkMTextCut "\\"
    @?= MTextUnsafe "\\"
  mkMTextCut "\\\\"
    @?= MTextUnsafe "\\\\"
  mkMTextCut "\""
    @?= MTextUnsafe "\""
  mkMTextCut "\\\""
    @?= MTextUnsafe "\\\""
  mkMTextCut "a\rb"
    @?= MTextUnsafe "ab"
  mkMTextCut "c\td\r"
    @?= MTextUnsafe "cd"
  mkMTextCut (toText @String [toEnum 5])
    @?= MTextUnsafe ""
  mkMTextCut (toText @String [toEnum 127])
    @?= MTextUnsafe ""
  mkMTextCut (toText @String [toEnum 300, 'A'])
    @?= MTextUnsafe "A"

unit_QuasiQuoter :: Assertion
unit_QuasiQuoter = do
  qqMText ""
    @?= Right ""
  qqMText "a ba"
    @?= Right "a ba"
  qqMText "a\nb"
    @?? isLeft
  qqMText "a\\nb"
    @?= Right "a\nb"
  qqMText "\\"
    @?? isLeft
  qqMText "\\\\"
    @?= Right "\\"
  qqMText "\""
    @?= Right "\""
  qqMText "\\\""
    @?? isLeft
  qqMText "\r"
    @?? isLeft
  qqMText "\t"
    @?? isLeft
  qqMText [toEnum 5]
    @?? isLeft
  qqMText [toEnum 127]
    @?? isLeft
  qqMText [toEnum 300]
    @?? isLeft

unit_mt :: Assertion
unit_mt = do
  [mt|aba|]
    @?= MTextUnsafe "aba"
  [mt| a  |]
    @?= MTextUnsafe " a  "
