import Test.Hspec (hspec)

import qualified Test.Parser as Parser

main :: IO ()
main = hspec $ do
  Parser.spec
