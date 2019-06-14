#!/usr/bin/env stack
{- stack
  script
  --resolver snapshot.yaml
  --package base
  --package text
  --package fmt
  --package hspec
  --package morley
-}

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module HelloTezosSpec where

import Data.Text (Text)
import Fmt (pretty)
import Test.Hspec (Spec, expectationFailure, hspec, it, shouldBe)

import Michelson.Test (contractProp, dummyContractEnv, specWithTypedContract)
import Michelson.Text (mt)
import Michelson.Typed (toVal)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specWithTypedContract "contracts/helloTezos.tz" $ \contract -> do
    it "Puts 'Hello Tezos!' to its storage" $
      contractProp contract validate' dummyContractEnv () [mt||]
  where
    validate' (res, _) =
      case res of
        Left err -> expectationFailure $
          "Unexpected contract failure: " <> pretty err
        Right (_operations, val) ->
          val `shouldBe` toVal [mt|Hello Tezos!|]
