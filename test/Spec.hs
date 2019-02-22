import Test.Hspec (hspec)

import qualified Test.Macro as Macro
import qualified Test.Parser as Parser
import qualified Test.Tezos.Crypto as Tezos.Crypto
import qualified Test.Typecheck as Typecheck

main :: IO ()
main = hspec $ do
  Parser.spec
  Macro.spec
  Typecheck.spec
  Tezos.Crypto.spec
