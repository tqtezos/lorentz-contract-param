# Morley Runtime

Morley runtime (`Michelson.Runtime*` modules) provides a high-level interface to Morley functionality.
It implements [high-level Morley interpreter](./morleyInterpreter.md#high-level-interpreter) and provides functions to read and parse a contract, originate it, transfer tokens, etc.

At present, Morley runtime doesn't communicate with any real Tezos network.
Instead, we emulate global blockchain state and store it on disk in a single JSON file.

## Blockchain State

A data type representing blockchain state is called `GState` (short for "Global State").
It stores a balance of each address, storage of each contract, and other data necessary to interpreter contracts.
Initially, the global state contains only one address with a lot of tokens which we call _genesis address_.

We currently use a simple JSON format to encode and decode this data type.
This format should be considered internal and may be changed later.
Probably, it will disappear completely at some point, and a real Tezos format will be used instead.
Nowadays you should use `morley` commands to update this state:
* `originate` command can be used to originate a contract.
This command reads a given contract, parses it, and typechecks it.
If the contract is well-typed, it's added to the global state, and its address is returned.
* `transfer` command can be used to change balances.
It sends a transaction from one address to another address.
It can also be used to update a storage value of a contract because a contract's code is executed when a transaction is sent to it.

Some data which is necessary to execute a contract and is not a part of a contract is not stored in `GState` but is passed via CLI:
* Gas limit is passed using the `--max-steps` option.
* Current timestamp (the result of the `NOW` instruction) is passed using the `--now` option.

Both options have default values.

For example, suppose you want to run a contract `a.tz` with the following blockchain state:
1. The contract `b.tz` should be originated, its balance should be 300, and its storage value should be `True`.
2. `a.tz` itself should have a balance of 500, and its storage should be 10.
3. The contract execution is allowed to consume at most 1000 gas.

In this case you should:
1. Run `morley originate --contract b.tz --storage True --balance 300` to originate `b.tz`. It should print the address of `b.tz`.
2. Run `morley originate --contract a.tz --storage 10 --balance 500` to originate `a.tz`. It should print the address of `a.tz`.
3. Run `morley transfer --parameter 10 --to KT1L39q6uCg1wQPB796q5oQQgDW673uo1s5y --max-steps 1000`, assuming `KT1L39q6uCg1wQPB796q5oQQgDW673uo1s5y` is the address of `a.tz`. It will actually execute `a.tz`. By default, the `transfer` command transfers 0 tokens. You can use the `--amount` option to specify a non-zero amount. It will be returned by the `AMOUNT` instruction. By default, tokens are sent from a genesis address which has a lot of money initially.

If later you want to transfer 10 tokens from `KT1L39q6uCg1wQPB796q5oQQgDW673uo1s5y` to `tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU` (just an arbitrary `tz1` address), you can use the `transfer` command again: `morley transfer --parameter Unit --to tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU --sender KT1L39q6uCg1wQPB796q5oQQgDW673uo1s5y --amount 10`. Notice that we pass `Unit` as a parameter because the destination address is the `tz1` address, it doesn't have any code.

It also should be noted that currently we ignore transaction fees (they are always 0).
