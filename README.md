# Morley: Developer tools for the Michelson Language

Morley is a library to make writing smart contracts in Michelson pleasant and
effective.

## I: A reimplementation of the Michelson Language in Haskell

It consists of the following parts:

- `Tezos.*` hierarchy is designed to implement cryptographic primitives, string and byte formats, and any other functionality specific to the Tezos protocol which is required for testing/execution of Michelson contracts but is used not only by Michelson.
- `Michelson.Untyped` and `Michelson.Typed` hierarchies define Haskell data types that assemble a Michelson contract. See [michelsonTypes.md](/docs/michelsonTypes.md).
- `Michelson.TypeCheck`: A typechecker that validates Michelson contracts according to the Michelson's typing rules. Essentially, it performs conversion from untyped representation to the typed one. See [morleyTypechecker.md](/docs/morleyTypechecker.md).
- `Michelson.Interpret`: An interpreter for Michelson contracts which doesn't perform any side effects. See [morleyInterpreter.md](/docs/morleyInterpreter.md).
- `Michelson.Types`: Types for macros, syntactic sugar, and other extensions that are described in the next chapter.
- `Michelson.Parser` A parser to turn a `.tz` or `.mtz` file (`.mtz` is a Michelson contract with Morley extensions) into a Haskell ADT.
- `Michelson.Runtime`: A high-level interface to Morley functionality, see [morleyRuntime.md](/docs/morleyRuntime.md).

## II: Morley extensions

One way to test Michelson contracts is to use the Morley language.
It is a superset of the Michelson language, which means that each Michelson contract is also a valid Morley contract but not vice versa.
There are several extensions which make it more convenient to write Michelson contracts and test them.
For example, one can write inline assertions in their contracts for testing.
All the details can be found in [the document](/docs/morleyLanguage.md) about these extensions.
Also, there is a transpiler from Morley to Michelson.

## III: Morley-to-Michelson transpiler

Morley-to-Michelson transpiler can be used to produce a Michelson contract from a Morley contract.
You should use it if you want to develop contracts in Morley and submit them to the Tezos network.
Workflow is the following:

1. If your contract is called `foo.mtz`, use `morley print --contract foo.mtz > foo.tz`. Note that normally you should not use `morley` directly, you should use `morley.sh` or `stack exec -- morley`. See usage instructions below.
2. After that, you can use existing Tezos tools to deploy your contract. You can also typecheck or interpret it using a reference implementation. If you are not familiar with the Tezos tooling, please read [Tezos documentation](http://tezos.gitlab.io/zeronet/index.html) or [Michelson tutorial](https://gitlab.com/morley-framework/michelson-tutorial).

## IV: Testing EDSL

Another way to test Michelson contracts is to write tests in Haskell using the testing EDSL provided by Morley.
It supports both integrational and unit tests.
Tests of both types can use static data or arbitrary data.
There is [a document](/docs/testingEDSL.md) with a detailed description of the EDSL and a tutorial about its usage.

## Running and building

Morley executable provides the following functionality:
- `parse` contract and return its representation in Haskell types.
- `typecheck` contract.
- `run` contract. A given contract is being originated first, and then the transaction is being sent to it
- `originate` contract.
- `transfer` tokens to a given address.
- `print` produce `.tz` contract that can be parsed by the OCaml referenced client from `.mtz` or `.tz` contract.

You can get more info about this command by running `morley <command> --help`

There are two ways to get Morley executable:
- [Docker](https://docs.docker.com/) based (preferable).
  * Get [script](/scripts/morley.sh)
 (e. g. using `curl https://gitlab.com/morley-framework/morley/raw/master/scripts/morley.sh > morley.sh`)
  and run it `./morley.sh <args>`. This script will pull a docker image that contains the latest version of Morley executable from the master branch and run it with the given arguments.
  * Usage example:
    + `./morley.sh` to see help message
    + `./morley.sh run --contract add1.tz --storage 1 --parameter 1 --amount 1`
- [Stack](https://docs.haskellstack.org/en/stable/README/) based.
  * Clone this git repository and run `stack build` command,
    after that you can do `stack exec -- morley <args>` to run morley executable built from the source code.
  * Usage example:
    + `stack exec -- morley --help` to see help message
    + `stack exec -- morley originate --contract contracts/add1.tz --storage 1 --verbose`

For more information about Morley commands, check out the following docs:
- [Interpreter doc](/docs/morleyInterpreter.md)
- [Typechecker doc](/docs/morleyTypechecker.md)

## Issue Tracker

We use [YouTrack](https://issues.serokell.io/issues/TM) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.
