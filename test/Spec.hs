import Test.Hspec (hspec)

import qualified Test.Parser as Parser
import qualified Test.Macro as Macro
import qualified Test.Typecheck as Typecheck

main :: IO ()
main = hspec $ do
  Parser.spec
  Macro.spec
  Typecheck.spec
