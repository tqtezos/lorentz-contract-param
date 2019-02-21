module Spec
  ( main
  ) where

import Test.Hspec (hspec)

import qualified Test.Macro as Macro
import qualified Test.Morley.Runtime as Morley.Runtime
import qualified Test.Parser as Parser
import qualified Test.Tezos.Crypto as Tezos.Crypto
import qualified Test.Typecheck as Typecheck

main :: IO ()
main = hspec $ do
  Parser.spec
  Macro.spec
  Typecheck.simpleSpec
  Typecheck.advancedSpec
  Tezos.Crypto.spec
  Morley.Runtime.spec
