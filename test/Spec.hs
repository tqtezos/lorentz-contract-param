import Test.Hspec (hspec)

import qualified Test.Parser as Parser
import qualified Test.Macro as Macro

main :: IO ()
main = hspec $ do
  Parser.spec
  Macro.spec
