module Main
  ( main
  ) where

import Test.Hspec (hspec)

import qualified Test.CValConversion as CVal
import qualified Test.Ext as Ext
import qualified Test.Interpreter as Interpreter
import qualified Test.Macro as Macro
import qualified Test.Michelson.Runtime as Michelson.Runtime
import qualified Test.Parser as Parser
import qualified Test.Printer.Michelson as Printer.Michelson
import qualified Test.Serialization.Aeson as Serialization.Aeson
import qualified Test.Serialization.Michelson as Serialization.Michelson
import qualified Test.Tezos.Address as Tezos.Address
import qualified Test.Tezos.Crypto as Tezos.Crypto
import qualified Test.Typecheck as Typecheck
import qualified Test.ValConversion as Val

main :: IO ()
main = hspec $ do
  Parser.spec
  Macro.spec
  Typecheck.typeCheckSpec
  Ext.typeCheckHandlerSpec
  Ext.interpretHandlerSpec
  Interpreter.spec
  Tezos.Crypto.spec
  Tezos.Address.spec
  Michelson.Runtime.spec
  Serialization.Aeson.spec
  Serialization.Michelson.spec
  Val.spec
  CVal.spec
  Printer.Michelson.spec
