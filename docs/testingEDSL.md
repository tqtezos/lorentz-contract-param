# EDSL for Testing Michelson Contracts in Haskell

One possible way to test your Michelson contracts is to write tests in Haskell using Morley Testing EDSL.
This document explains how to do it.
The advantages of this approach are the following:
1. You can use the full power of Haskell libraries, such as [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck), [`hspec`](https://hackage.haskell.org/package/hspec), etc.
2. Haskell has a lot of tooling built around it: code formatters, linters, editor plugins, etc.
It makes writing code in Haskell quite pleasant and convenient.
3. Haskell syntax is quite concise and expressive, so tests written in Haskell are easy to read even for a person with little knowledge of the language.

## Table of contents

- [Prerequisites](#prerequisites)
- [Overview](#overview)
- [Write Your First Tests](#write-your-first-tests)
  * [Hello Tezos](#hello-tezos)
  * [Property Testing](#property-testing)
- [Integrational Testing](#integrational-testing)
  * [Arbitrary Blockchain State](#arbitrary-blockchain-state)
- [Managing Multiple Tests](#managing-multiple-tests)
- [Summary](#summary)

## Prerequisites

1. The reader should be familiar with the basic features of Haskell. Be able to define functions and data types, use pattern matching, guards, case expressions, do-notation, etc. Know fundamental types and functions from `base`.
2. Familiarity with testing libraries like `QuickCheck` and `hspec` is also desirable.
3. In this document, we will use [`stack`](https://docs.haskellstack.org/en/stable/README/) to build Haskell code.

## Overview

Morley is essentially a set of developer tools for the Michelson Language written in Haskell.
In particular, it contains an EDSL to write tests for Michelson contracts.
There are two types of tests one can write:
* Unit tests.
They consider only one contract and do not consider the fact that a contract can originate or call other contracts.
They also ignore modifications of the global blockchain state that a contract can make.
* Integrational tests.
These tests, on the other hand, let you perform operations on multiple contracts and check predicates about the global blockchain state.
For example, one can originate two contracts which call each other, and the testing engine will execute them properly.

At present, slightly different interfaces are used for these types of tests, but internally they use the same interpreter implementation to run contracts.
Both types of tests can work with static data or with randomly generated data (in this case we call them _property-based_ tests).

## Write Your First Tests

*Disclaimer: following examples correspond to [version 0.2.0.1](http://hackage.haskell.org/package/morley-0.2.0.1) and may be outdated.*

This chapter provides a step-by-step guide to writing tests using EDSL.
We start with a very simple contract and test case and then proceed to more advanced features.
All files mentioned in this chapter can be found in the [`examples/EDSL`](/examples/EDSL) directory.
You can use [Haddock documentation on Hackage](https://hackage.haskell.org/package/morley) to read more information about functions and data types used here.

### Hello Tezos

We start with a contract which unconditionally puts "Hello Tezos!" string into its storage:

```
# helloTezos.tz
parameter unit;
storage string;
code {DROP;
      PUSH string "Hello Tezos!";
      NIL operation; PAIR;};
```

Let's create a directory called `contracts/` and put this contract into `contracts/helloTezos.tz`.
In this example, we will not create a full Stack project but instead will use the ["script interpreter"](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter) feature of Stack.

Let's create a file called `HelloTezosSpec.hs` with a unit test for our `helloTezos.tz` contract.
Here is the full test suite:

```haskell
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
```

It starts with a shebang line to execute `stack` and arguments that will be passed to the `stack` executable.
We are using the following packages:
* `base`, `text` are the most basic Haskell packages which are used almost everywhere.
* `fmt` is used for pretty printing.
* `hspec` is a generic Haskell testing framework that we use to setup testing infrastructure.
* `morley` is the Morley library itself.

Then we enable `OverloadedStrings` to be able to write `Text` constants and `QuasiQuotes` to create Michelson strings using `mt`.
Then we import some modules.
All test logic is in `spec` which has type `Spec`.
You can treat it as a specification of a contract we want to test.
The test itself does the following:
1. It imports the contract from `contracts/helloTezos.tz` using `specWithTypedContract` which takes a callback as its argument.
This callback's argument is the Haskell representation of the imported contract.
It uses _typed_ representation of the contract as described in [another document](./michelsonTypes.md).
2. Then it uses `hspec`'s `it` function to create a spec item.
It takes a textual description and an example.
In our case, the only behavior we want to describe is that the contract puts a certain string to its storage, so it's our description.
3. The example is defined using the `contractProp` function from `Morley.Test`.
It takes a contract, a validation function, environment, contract's parameter, and initial storage.
Environment essentially contains blockchain state which is irrelevant for this test, so we just use a dummy value (`dummyContractEnv`).
The contract's parameter is `unit` and the storage type is `string`.
We pass `()` and empty string as parameter and storage respectively, and they get automatically converted to Michelson values.
4. The most interesting part is the validation function.
It takes a pair of values.
The first value is `Either` an error (which corresponds to the `[FAILED]` state from Michelson) or a pair which contains a list of operations (`operation` type in Michelson) and a final storage value.
The second value is the final interpreter state, which is not essential for us now (it can be used to figure out the amount of gas that was consumed by a contract, for example).
If the contract fails, we use `hspec`'s `expectationFailure` because the contract's failure is not what we expect.
Otherwise, we check that resulting storage value is `"Hello Tezos!"`.

Notice that we use `toVal` to convert `[mt|Hello Tezos!|]`, which has type `MText`, to a Michelson value.
It's a polymorphic function which converts various Haskell values to Michelson values.
Michelson value type is a GADT defined in `Michelson.Typed`.
It's not necessary to understand its internals in order to use this EDSL, but it might be useful to know how it works under the hood.

Now we can launch our test:
> stack HelloTezosSpec.hs

Alternatively you can do `chmod +x HelloTezosSpec.hs` and run `./HelloTezosSpec.hs`.

Please note that it may take a while to download all the dependencies and compile them.
In the end, you should see the following output:

```
Test contract contracts/helloTezos.tz
  Puts 'Hello Tezos!' to its storage

Finished in 0.0005 seconds
1 example, 0 failures
```

### Property Testing

Now let's write a more advanced test.
This time we'll write a property-based test using `QuickCheck`.

Let's test the following contract:
```
# compare.tz
# Accepts a pair of values of type Mutez (Pair a b).
# Returns a bool list:
# [ a == b?
# , a >  b?
# , a <  b?
# , a >= b?
# , a <= b?
# ]

parameter (pair mutez mutez);
storage (list bool);
code {CAR; DUP; DUP; DUP; DUP; DIIIIIP {NIL bool};
      DIIIIP {DUP; CAR; DIP {CDR}; COMPARE; LE; CONS};
      DIIIP {DUP; CAR; DIP {CDR}; COMPARE; GE; CONS};
      DIIP{DUP; CAR; DIP {CDR}; COMPARE; LT; CONS};
      DIP {DUP; CAR; DIP {CDR}; COMPARE; GT; CONS};
      DUP; CAR; DIP {CDR}; COMPARE; EQ; CONS;
      NIL operation; PAIR};
```

Let's put it into `contracts/compare.tz`.

As you can see, it takes a pair of `Mutez` values and puts a `list` of `bool`s into its storage.
There are 5 values as described in the comment.

Again let's see our test straight away:

```haskell
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
```

It starts with the same lines, except that a module name is different and now we are using the `QuickCheck` library as well.
We also define type aliases for Haskell types corresponding to the contract's parameter and storage types.

Now let's look at the body of the spec.
* It looks quite similar, but now we are using the `prop` function from `hspec`.
* We pass a lambda to it which takes `inputParam` as its argument.
This `inputParam` will be generated by `QuickCheck` using the `Arbitrary` instance for `Parameter` (i. e. `(Mutez, Mutez)`).
This instance is located in the `Morley.Test.Gen` module.
* Inside this lambda we are using `contractProp`.
It's the same function that we used in the first example.
Notice that it can be used not only within `it`, but also within `prop`.
* We pass the same set of arguments to this function.
Initial storage doesn't matter, as well as the environment.
A parameter, on the other hand, is essential for this script.
We pass generated `inputParam` as contract's parameter to `contractProp`.
We also pass it to the validation function.
* Let's take a closer look at the validation function.
It takes a parameter that was passed to the contract and the result of the contract's execution.
This time we've provided its type explicitly.
`ContractResult` is a type alias defined in `Michelson.Interpret`: `(Either MichelsonFailed ([Operation Instr], Val Instr st), InterpreterState s)`.
Don't be scared when you see the `ToT Storage` thing, `ToT` is a type family which maps plain Haskell types to the Haskell representation of Michelson types.
The validation function expects that the contract execution will succeed returning an empty list of operations and the final storage will be the same as the result of `mkExpected` applied to the parameter.
If it differs, the test fails.

You can run this test the same way as `HelloTezos.hs` and you should see the following output:
```
Test contract contracts/compare.tz
  Random check
    +++ OK, passed 100 tests.

Finished in 0.0018 seconds
1 example, 0 failures
```

## Integrational Testing

Now let's get familiar with another machinery for writing tests: integrational testing EDSL.
In our first example we will use two contracts `stringCaller.tz` and `failOrStoreAndTransfer.tz`:

```
# stringCaller.tz
# This contract takes a string as parameter and an address as storage.
# It transfers 300 mutez to the given address and passes its parameter as
# parameter for this transfer.
# It fails if current timestamp is greater than 500.

parameter string;
storage address;
code {
       # Check current timestamp
       PUSH timestamp 500;
       NOW;
       IFCMPGT { FAIL; } { };
       # Construct operations
       DUP;
       DUP;
       CDR;
       CONTRACT string;
       IF_NONE {DROP; NIL operation }
               {SWAP;
                CAR;
                DIP {PUSH mutez 300};
                TRANSFER_TOKENS;
                DIP {NIL operation;};
                CONS;
               };
       DIP { CDR };
       PAIR;
     };

```

```
# failOrStoreAndTransfer.tz
# This contract takes a string as parameter and updates its storage to
# this string.
# However, it fails if its balance is greater than 1000.
# Also it transfers 5 mutez to a fixed address (tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU).

parameter string;
storage string;
code { CAR; # ignore storage
       # Check balance and possibly fail
       PUSH mutez 1000;
       BALANCE;
       IFCMPGT { FAIL; } { };
       # Construct transfer operation
       NIL operation;
       PUSH (contract unit) "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU";
       PUSH mutez 5;
       UNIT;
       TRANSFER_TOKENS;
       CONS;
       PAIR;};
```

Their behavior is pretty well described in their comments.
The test is located in `StringCaller.tz`.
Let's skip some boilerplate from the beginning of this file and look at its `spec`:

```haskell
spec :: Spec
spec =
  parallel $
  specWithContract "contracts/stringCaller.tz" $ \stringCaller ->
  specWithContract "contracts/failOrStoreAndTransfer.tz" $ \failOrStoreAndTransfer ->
  specImpl stringCaller failOrStoreAndTransfer
```

The difference from the previous tests is that it uses `specWithContract` instead of `specWithTypedContract`.
It provides two representations of the same contract: a typed and untyped one.
You can read more about it in [a document about Michelson types](./michelsonTypes.md).
We import two contracts and pass them to `specImpl`:

```haskell
specImpl ::
     (Untyped.Contract, Contract (ToT MText) (ToT Address))
  -> (Untyped.Contract, Contract (ToT MText) (ToT MText))
  -> Spec
specImpl (uStringCaller, _stringCaller) (uFailOrStore, _failOrStoreAndTransfer) = do
  let scenario = integrationalScenario uStringCaller uFailOrStore
  let prefix =
        "stringCaller calls failOrStoreAndTransfer and updates its storage with "
  let suffix =
        " and properly updates balances. But fails if failOrStoreAndTransfer's"
        <> " balance is ≥ 1000 and NOW is ≥ 500"
  it (prefix <> "a constant" <> suffix) $
    integrationalTestExpectation (scenario constStr)

  -- The test is trivial, so it's kinda useless to run it many times
  modifyMaxSuccess (const 2) $
    prop (prefix <> "an arbitrary value" <> suffix) $
      \str -> integrationalTestProperty (scenario str)
  where
    constStr = [mt|caller]
```

First of all, let's look at its type.
Both arguments are pairs of untyped and typed contracts.
Typed contract's type defines expected parameter and storage types of imported contracts.
So the first contract's parameter has type `string` (corresponds to Haskell's `Text`) and storage has type `address` (corresponding to Haskell's `Address`).
The second contract's parameter and storage both have type `string`.

The body of `specImpl` contains two tests: the first one starts with `it` and the second one starts with `prop`.
Both of them use `scenario = integrationalScenario uStringCaller uFailOrStore` to specify testing logic.
The first test uses `integrationalTestExpectation` and the second one uses `integrationalTestProperty`.
These functions have the same semantics, but the first one returns `Expectation` and the second one returns `Property`.
The second test is property-based, we use `modifyMaxSuccess` to run it at most twice, because the test is rather simple.
The most interesting part happens in `integrationalScenario` that is defined below:

```haskell
integrationalScenario :: Untyped.Contract -> Untyped.Contract -> MText -> IntegrationalScenario
integrationalScenario stringCaller failOrStoreAndTransfer str = do
  let
    initFailOrStoreBalance = unsafeMkMutez 900
    initStringCallerBalance = unsafeMkMutez 500

  -- Originate both contracts
  failOrStoreAndTransferAddress <-
    originate failOrStoreAndTransfer "failOrStoreAndTransfer" (Untyped.ValueString [mt|hello|]) initFailOrStoreBalance
  stringCallerAddress <-
    originate stringCaller "stringCaller"
    (Untyped.ValueString $ mformatAddress failOrStoreAndTransferAddress)
    initStringCallerBalance

  -- NOW = 500, so stringCaller shouldn't fail
  setNow (timestampFromSeconds (500 :: Int))
```

First, we originate both contracts.
We need to supply initial balance and storage value for each contract.
We pass the second contract's address to `stringCaller` so that `stringCaller` will call `failOrStoreAndTransfer` every time it's called.
Then we set the current timestamp to 500 to ensure that `stringCaller` won't fail.
Now let's transfer 100 tokens to `stringCaller`:


```haskell
  -- Transfer 100 tokens to stringCaller, it should transfer 300 tokens
  -- to failOrStoreAndTransfer
  let
    newValue = Untyped.ValueString str
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = newValue
      , tdAmount = unsafeMkMutez 100
      }
    transferToStringCaller = transfer txData stringCallerAddress
  transferToStringCaller
```

This transfer should succeed.
* `stringCaller` should receive 100 tokens and send 300 tokens.
* `failOrStoreAndTransfer` should receive 300 tokens and spend 5 tokens.
* `tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"` should receive 5 tokens.
* Storage of `failOrStoreAndTransferAddress` should be updated to `str`.

```haskell
  -- Execute operations and check balances and storage of 'failOrStoreAndTransfer'
  do
    let
      -- `stringCaller.tz` transfers 300 mutez.
      -- 'failOrStoreAndTransfer.tz' transfers 5 tokens.
      -- Also 100 tokens are transferred from the genesis address.
      expectedStringCallerBalance = unsafeMkMutez (500 - 300 + 100)
      expectedFailOrStoreBalance = unsafeMkMutez (900 + 300 - 5)
      expectedConstAddrBalance = unsafeMkMutez 5

      updatesValidator :: SuccessValidator
      updatesValidator = composeValidatorsList
        [ expectStorageUpdateConst failOrStoreAndTransferAddress newValue
        , expectBalance failOrStoreAndTransferAddress expectedFailOrStoreBalance
        , expectBalance stringCallerAddress expectedStringCallerBalance
        , expectBalance constAddr expectedConstAddrBalance
        ]
    validate (Right updatesValidator)
```

You can see how we constructed a validator that checks everything above.
`constAddr` is defined below as `unsafeParseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"`.
The validator expects successful invocation of commands.
`validate` takes `Either (InterpreterError -> Bool) SuccessValidator` as argument.
We pass `Right updatesValidator` and `updatesValidator` is `SuccessValidator`.
However, we can also expect interpreter failure. Let's try to do the same transfer again.
It should fail, because `failOrStoreAndTransfer` fails when its balance is greater than 1000:

```haskell
  -- Now let's transfer 100 tokens to stringCaller again.
  transferToStringCaller

  -- This time execution should fail, because failOrStoreAndTransfer should fail
  -- because its balance is greater than 1000.
  validate (Left $ expectMichelsonFailed (const True) failOrStoreAndTransferAddress)
```

In this case we expect that `failOrStoreAndTransfer` will reach `[FAILED]` state.

It's not necessary to transfer to a contract's address, we can transfer to `tz1` address as well:

```haskell
  -- We can also send tokens from failOrStoreAndTransfer to tz1 address directly
  let
    txDataToConst = TxData
      { tdSenderAddress = failOrStoreAndTransferAddress
      , tdParameter = Untyped.ValueUnit
      , tdAmount = unsafeMkMutez 200
      }
  transfer txDataToConst constAddr
```

After this operation, the balance of `constrAddr` should increase by 200 and the balance of `failOrStoreAndTransfer` should decrease by 200.
Let's check it:

```haskell
  -- Let's check balance of failOrStoreAndTransfer and tz1 address
  do
    let
      expectedFailOrStoreBalance = unsafeMkMutez (900 + 300 - 5 - 200)
      expectedConstAddrBalance = unsafeMkMutez (5 + 200)

      updatesValidator :: SuccessValidator
      updatesValidator = composeValidatorsList
        [ expectBalance failOrStoreAndTransferAddress expectedFailOrStoreBalance
        , expectBalance constAddr expectedConstAddrBalance
        ]

    validate (Right updatesValidator)
```

Now let's transfer to `stringCaller` again, it should succeed because the balance of `failOrStoreAndTransfer` is ≤ 1000 now.
Then let's set the current timestamp to 600.
After that `stringCaller` should fail because the current timestamp is greater than 500.

```haskell
  -- Now we can transfer to stringCaller again and it should succeed
  -- this time, because the balance of failOrStoreAndTransfer decreased
  transferToStringCaller

  -- Let's simply assert that it should succeed to keep the scenario shorter
  validate (Right expectAnySuccess)

  -- Now let's set NOW to 600 and expect stringCaller to fail
  setNow (timestampFromSeconds (600 :: Int))
  transferToStringCaller
  validate (Left $ expectMichelsonFailed (const True) stringCallerAddress)
```

This is the end of this test, but in principle we can continue performing operations and validating their effects.
Note that operations are executed only when we call `validate`.
It allows us to batch operations and execute them together.
There is one restriction: each scenario must end with `validate`, because that's the only way to execute an operation.

As usual, you can run this test using `stack StringCallerSpec.hs`.

### Arbitrary Blockchain State

In our last example, we'll demonstrate how to test a contract against arbitrary blockchain state.
It should be noted that the amount of blockchain state that a contract can use is quite limited, so there is not much data that can be generated.
Also, it's not very useful to generate completely arbitrary state, because most of it will most likely be irrelevant to a contract.
We'll use a contract which behavior heavily depends on the blockchain state.
Let's call it `environment.tz`:

```
# envrionment.tz

# This contract's behavior heavily depends on the environment in which it's executed.
# 1. It fails if its balance is greater than 1000.
# 2. It fails if NOW is less than 100500.
# 3. It fails if the address passed to it is a contract with parameter `address`.
# 4. It fails if the amount transferred to it is less than 15.
# 5. Its final storage value is whether remaining gas (almost) in the end is greater than 1000.
parameter address;
storage bool;
code {
       # Check balance and possibly fail
       PUSH mutez 1000;
       BALANCE;
       IFCMPGT { BALANCE; FAILWITH; } { };

       # Check NOW and possibly fail
       PUSH timestamp 100500;
       NOW;
       IFCMPLT { NOW; FAILWITH; } { };

       # Check address passed as parameter
       CAR;
       CONTRACT address;
       IF_SOME { FAILWITH; } { };

       # Check amount transferred to this contract
       PUSH mutez 15;
       AMOUNT;
       IFCMPLT { AMOUNT; FAILWITH; } { };

       # Update storage depending on STEPS_TO_QUOTA
       PUSH nat 1000;
       STEPS_TO_QUOTA;
       CMPGT;

       # Finish
       NIL operation;
       PAIR; };
```

Its behavior depends on the following values:
1. Its balance.
2. Current timestamp
3. Whether address passed to it as parameter is originated with parameter type `address`.
4. Amount transferred to this contract.
5. Gas limit at the beginning of this contract's execution.

Now let's look at the test contained in `EnvironmentSpec.hs`.
Again let's omit all the initial boilerplate.
`spec` is defined simply as `specWithContract "contracts/environment.tz" specImpl`.
Then we define a data type that we call `Fixture`:

```haskell
data Fixture = Fixture
  { fNow :: !Timestamp
  , fMaxSteps :: !RemainingSteps
  , fPassOriginatedAddress :: !Bool
  , fBalance :: !Mutez
  , fAmount :: !Mutez
  } deriving (Show)
```

This data type contains all data that is a part of the blockchain state and is relevant for our contract.
`fPassOriginatedAddress` determines whether we will pass an originated address with parameter `address` to this contract as parameter.
In our case, we will originate `environment.tz` itself and will pass its address to itself if `fPassOriginatedAddress` is `True`.
All this data will be generated by QuickCheck.
We define `Arbitrary` instance which specifies how exactly this data will be generated:

```haskell
instance Arbitrary Fixture where
  arbitrary = do
    fNow <- timestampFromSeconds <$> choose (100000 :: Int, 111111)
    fMaxSteps <- RemainingSteps <$> choose (1015, 1028)
    fPassOriginatedAddress <- arbitrary
    fBalance <- unsafeMkMutez <$> choose (1, 1234)
    fAmount <- unsafeMkMutez <$> choose (1, 42)
    return Fixture {..}
```

For most of values we use QuickCheck's `choose` function which picks an arbitrary value in some range.
For boolean `fPassOriginatedAddress` we just use `arbitrary` which will generate `True` or `False`.
In principle, we can define as complex generators as we want.
Then we define two functions which represent contract's behavior:

```haskell
shouldExpectFailed :: Fixture -> Bool
shouldExpectFailed fixture =
  or
    [ fBalance fixture > unsafeMkMutez 1000
    , fNow fixture < timestampFromSeconds (100500 :: Int)
    , fPassOriginatedAddress fixture
    , fAmount fixture < unsafeMkMutez 15
    ]

shouldReturn :: Fixture -> Untyped.Value
shouldReturn fixture
  | fMaxSteps fixture - consumedGas > 1000 = Untyped.ValueTrue
  | otherwise = Untyped.ValueFalse
  where
    consumedGas = 19
```

`shouldExpectFailed` returns whether the contract should fail and `shouldReturn` returns the value that the contract should return when it succeeds.
`consumedGas` in the definition of `shouldReturn` is the amount of gas that this contract consumes before it reaches the `STEPS_TO_QUOTA` instruction.

Our `specImpl` looks similar to `specImpl` from the previous test case:
```haskell
specImpl ::
    (Untyped.Contract, Contract (ToT Address) (ToT Bool))
  -> Spec
specImpl (uEnvironment, _environment)  = do
  let scenario = integrationalScenario uEnvironment
  prop description $
    integrationalTestExpectation . scenario
  where
    description =
      "This contract fails under conditions described in a comment at the " <>
      "beginning of this contract and returns whether remaining gas is " <>
      "greater than 1000"
```

`scenario` takes `Fixture` as an argument, so we pass a function that takes `Fixture` argument to `prop description`.
In this case, `Fixture` will be generated by QuickCheck using `Arbitrary` instance.
All logic is defined in `integrationalScenario` again, so let's see it:

```haskell
integrationalScenario :: Untyped.Contract -> Fixture -> IntegrationalScenario
integrationalScenario contract fixture = do
  -- First of all let's set desired gas limit and NOW
  setNow $ fNow fixture
  setMaxSteps $ fMaxSteps fixture

  -- Then let's originate the 'environment.tz' contract
  environmentAddress <-
    originate contract "environment" Untyped.ValueFalse (fBalance fixture)

  -- And transfer tokens to it
  let
    param
      | fPassOriginatedAddress fixture = environmentAddress
      | otherwise = genesisAddress
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = Untyped.ValueString (mformatAddress param)
      , tdAmount = fAmount fixture
      }
  transfer txData environmentAddress

  -- Execute operations and check that interpreter fails when one of
  -- failure conditions is met or updates environment's storage
  -- approriately
  let
    validator
      | shouldExpectFailed fixture =
        Left $ expectMichelsonFailed (const True) environmentAddress
      | otherwise =
        Right $ expectStorageConst environmentAddress $ shouldReturn fixture
  validate validator
```

Essentially we do the following:
1. Setup desirable blockchain state for testing: set current timestamp and gas limit, originate our contract.
2. Then we create transaction data based on the fixture.
If `fPassOriginatedAddress` we pass the address of `environment.tz`, otherwise we pass genesis address which is just some hardcoded `tz1` address.
3. Then we construct a validator which expects failure if `shouldExpectFailed fixture` is `True` and expects success otherwise.
In the success scenario `environmentAddress` should have storage value equal to `shouldReturn fixture`.

Now we can do `stack EnvironmentSpec.hs` to run this test.

## Managing Multiple Tests

When you have more tests for more contracts, it will be inconvenient to maintain them manually as a bunch of Haskell scripts.
To overcome this inconvenience, you can create a complete Stack project and put all your tests there.

The [folder with these examples](/examples/EDSL) contains a Stack project with all example tests.
All we need to do is to create a simple `.cabal` file (see [`edsl-demo.cabal`](/examples/EDSL/edsl-demo.cabal)), specify `Spec.hs` as `main-is` and put `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` into `Spec.hs`.
`hspec` will automatically find all `*Spec` files and include them into our test-suite.
Now we can run all tests using `stack test`.

## Summary

In this document, we have demonstrated how one can write tests for their Michelson contracts in Haskell.
We have started with a simple unit test, then demonstrated a slightly more complex property-based unit test, and then two integrational tests.

* Both unit and integrational tests start with `specWithContract` or `specWithTypedContract` to import a contract from a file.
It's used to create `hspec`'s `Spec`.
It can be used many times to import multiple contracts in integrational tests.
* After that we use `it` for tests with static data and `prop` for property-based tests.
* In unit tests, we use `contractProp` to which we pass a contract, environment, parameter, and storage.
All of this data can be static or arbitrary.
More importantly, it also takes a validator for the contract's result.
* In integrational tests, we use `integrationalTestExpectation` or `integrationalTestProperty`.
Both of them take `IntegrationalScenario` as argument.
In this scenario, we can use commands like `transfer` and `originate` and validate various assertions about the current blockchain state.

An interested reader can find more examples in our test suite that we use to test `morley` itself.
Specifically, tests for contracts are located in `Test.Interepter.*` modules of the `morley-test` test suite.

In the end, we want to notice that it's only an alpha version of our EDSL.
We have many ideas about making it better, and probably it will substantially change in the future.
