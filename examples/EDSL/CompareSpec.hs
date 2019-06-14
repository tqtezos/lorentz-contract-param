#!/usr/bin/env stack
{- stack
  script
  --resolver snapshot.yaml
  --package base
  --package text
  --package fmt
  --package hspec
  --package QuickCheck
  --package morley
-}

{-# LANGUAGE OverloadedStrings #-}

module CompareSpec where

import Fmt (pretty)
import Test.Hspec (Spec, hspec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===))

import Michelson.Test
  (ContractReturn, contractProp, dummyContractEnv, failedProp, specWithTypedContract)
import Michelson.Typed (ToT, fromVal)
import Tezos.Core (Mutez)

type Parameter = (Mutez, Mutez)
type Storage = [Bool]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specWithTypedContract "contracts/compare.tz" $ \contract -> do
    prop "Random check" $ \inputParam ->
      contractProp contract (validate inputParam)
      dummyContractEnv inputParam initStorage
  where
    initStorage :: Storage
    initStorage = []

    mkExpected :: Parameter -> Storage
    mkExpected (a, b) = [a == b, a > b, a < b, a >= b, a <= b]

    validate
      :: Parameter
      -> ContractReturn (ToT Storage)
      -> Property
    validate p (Right ([], l), _) = fromVal l === mkExpected p
    validate _ (Left e, _) =
      failedProp $ "Unexpected failure of the sctipt: " <> pretty e
    validate _ _ =
      failedProp "Invalid result of the script"
