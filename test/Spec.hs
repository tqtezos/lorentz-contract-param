module Main
  ( main
  ) where

import Test.Hspec (hspec)

import qualified Test.Interpreter as Interpreter
import qualified Test.Macro as Macro
import qualified Test.Nop as Nop
import qualified Test.Morley.Runtime as Morley.Runtime
import qualified Test.Parser as Parser
import qualified Test.Serialization.Aeson as Serialization.Aeson
import qualified Test.Tezos.Crypto as Tezos.Crypto
import qualified Test.Typecheck as Typecheck

main :: IO ()
main = hspec $ do
  Parser.spec
  Macro.spec
  Typecheck.typeCheckSpec
  Nop.nopHandlerSpec
  Interpreter.spec
  Tezos.Crypto.spec
  Morley.Runtime.spec
  Serialization.Aeson.spec
