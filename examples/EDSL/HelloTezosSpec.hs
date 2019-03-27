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

{-# LANGUAGE OverloadedStrings #-}

module HelloTezosSpec where

import Data.Text (Text)
import Fmt (pretty)
import Test.Hspec (Spec, expectationFailure, hspec, it, shouldBe)

import Michelson.Typed (toVal)
import Morley.Test (contractProp, dummyContractEnv, specWithTypedContract)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specWithTypedContract "contracts/helloTezos.tz" $ \contract -> do
    it "Puts 'Hello Tezos!' to its storage" $
      contractProp contract validate' dummyContractEnv () ("" :: Text)
  where
    validate' (res, _) =
      case res of
        Left err -> expectationFailure $
          "Unexpected contract failure: " <> pretty err
        Right (_operations, val) ->
          val `shouldBe` toVal ("Hello Tezos!" :: Text)
