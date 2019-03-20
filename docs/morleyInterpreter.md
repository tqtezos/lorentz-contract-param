# Morley Interpreter

Morley interpreter is implemented in two layers:
* An interpreter for the core Michelson language which doesn't perform any side effects.
It means that it always interprets only one contract.
It's located in `Michelson.Interpret`.
* High-level Morley interpreter that interpreters contracts using Michelson intepreter and also interprets side effects.
It's located in `Morley.Runtime`.

## Michelson interpreter

Michelson interpreter simply implements the specification of the Michelson language.
It's implemented as a pure function which takes all necessary data as pure Haskell values stored inside the `ContractEnv` data type.
It also takes a handler for extra instructions described in the [`morleyInstructions.md`](./morleyInstructions.md) document.
This design allows the interpreter to work in any environment: it can work with real blockchain data, with data supplied by the user, with randomly generated data, etc.

There are some limitations of this interpreter which are going to be resolved later:
* [TM-35](https://issues.serokell.io/issue/TM-35) `PACK` and `UNPACK` instructions are not implemented.
* [TM-62](https://issues.serokell.io/issue/TM-62) Contract's address is computed improperly (it uses essentially the same logic, but different binary format).
* [TM-80](https://issues.serokell.io/issue/TM-80) Each instruction consumes 1 gas.
* [TM-81](https://issues.serokell.io/issue/TM-81) Not all signing schemes are supported.

This list may be outdated, because we may find discover new issues or fix existing ones.
You can see all unresolved issues in our [issue tracker](https://issues.serokell.io/issues?q=project:%20%7BTezos%20Michelson%7D%20%23Unresolved).

## High-level interpreter

High-level interpreter has the following goals:
1. Interpret operations returned by contracts.
An operation can originate a new contract or call another contract by sending a transaction to its address.
2. Actually perform side effects.
For example, it needs to update storages of all executed contracts and their balances.

At present, high-level interpreter doesn't communicate with any real Tezos network.
Instead, we emulate global blockchain state and store it on disk in a single JSON file.
It stores balances of each address, storages of each contract and other data necessary for the interpreter.

End user can use one of the following commands:
* `originate` command reads a contract, parses and type checks it.
If the contract is well-typed, it's added to the global state and its address is returned.
* `transfer` command sends a transaction from one address to another address.
If destination address is a contract with some code, this code is executed.
Its storage will be updated and it may return some operations which will also be interpreted.
* `run` command essentially combines `originate` and `transfer` commands.
By default it doesn't update global state.
It's intended to be used when one just wants to quickly interpret one contract and doesn't care about global state file.
I still needs to read DB to know about all originated contracts and their parameters.
It's needed for the `contract` command.
Other environment can be passed from CLI, but it's not mandatory, default values are used if their are not passed explicitly.

Initially global state contains only one address with a lot of tokens and each operation by default uses this address as sender.
We do not require signatures for transactions, anyone can spend tokens without any private keys, because it's irrelevant to Michelson.
