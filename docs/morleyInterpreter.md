# Morley Interpreter

Morley interpreter takes a well-typed Morley contract (in [typed representation](./michelsonTypes.md#typed-types)), interprets all its instructions according to the Michelson specification and then performs side effects.
There can be the following side effects:
1. Update storage value of a contract.
2. Update balance of an address.
3. An operation returned by a contract (`operation` type from Michelson).

Morley interpreter is implemented in two layers:
* An interpreter for the core Michelson language which doesn't perform any side effects.
It means that it always interprets only one contract.
It's located in `Michelson.Interpret`.
* High-level Morley interpreter that interpreters contracts using Michelson interpreter and also interprets side effects.
It's located in `Morley.Runtime`.

## Michelson interpreter

Michelson interpreter simply implements the specification of the Michelson language.
It's implemented as a pure function which takes all necessary data as pure Haskell values stored inside a Haskell data type called `ContractEnv`.
It also takes a handler for extra instructions described in the [`morleyInstructions.md`](./morleyInstructions.md) document.
This design allows the interpreter to work in any environment: it can work with real blockchain data, with data supplied by the user, with randomly generated data, etc.

There are some limitations of this interpreter which are going to be resolved later:
* [TM-35](https://issues.serokell.io/issue/TM-35) `PACK` and `UNPACK` instructions are not implemented.
* [TM-62](https://issues.serokell.io/issue/TM-62) Contract's address is computed improperly (it uses essentially the same logic, but different binary format).
* [TM-80](https://issues.serokell.io/issue/TM-80) Each instruction consumes 1 gas.
* [TM-81](https://issues.serokell.io/issue/TM-81) Not all signing schemes are supported.

This list might be outdated because we may discover new issues or fix the existing ones.
You can see all the unresolved issues in our [issue tracker](https://issues.serokell.io/issues?q=project:%20%7BTezos%20Michelson%7D%20%23Unresolved).

## High-level interpreter

High-level interpreter works with [global blockchain state](./morleyRuntime.md#blockchain-state).
This interpreter has the following goals:
1. Interpret operations returned by contracts.
An operation can originate a new contract or call another contract by sending a transaction to its address.
2. Perform other side effects: update storages of all the executed contracts and their balances.
3. Write an updated blockchain state on disk.

An end user can apply one of the following commands to execute the interpreter:
* `transfer` command sends a transaction from one address to another address.
If the destination address is a contract with some code, this code will be executed.
Its storage will be updated, and it may return some operations which will also be interpreted.
The contract must be originated first.
* `run` command originates a contract and transfers tokens to it.
By default, it doesn't update the global state.
It's intended to be used when one just wants to quickly interpret one contract and doesn't care about a global state file.
The command still needs to read the DB (JSON file) to know about all originated contracts and their parameters.
It's needed for the `CONTRACT` instruction.
Other environments can be passed from CLI, but it's not mandatory, and default values are used if they are not passed explicitly.

Examples:
* `morley transfer --to KT1L39q6uCg1wQPB796q5oQQgDW673uo1s5y --parameter 'Left 10'` (`KT1L39q6uCg1wQPB796q5oQQgDW673uo1s5y` must be originated first).
* `morley run --contract foo.tz --parameter '"aaa"' --storage Unit` (in this case the contract will be originated automatically).

By default, each operation uses _genesis address_ as a sender.
This address initially has a lot of tokens.
We do not require signatures for transactions, anyone can spend tokens without any private keys because it's irrelevant to Michelson.
