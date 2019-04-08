# Michelson Types

We have two representations of types that assemble a Michelson contract: typed and untyped representations.
The former is located in the `Michelson.Typed` module hierarchy, the latter is in `Michelson.Untyped`.

Note that in whole `Michelson` module hierarchy we consider only the core Michelson language, i. e. only constructions that may appear in the Tezos blockchain.
It means that macros are not represented there, they are considered at a higher level.
We have some extensions of the language though (see [`morleyInstructions.md`](./morleyInstructions.md)), so we have an additional `Ext` instruction.
Its particular definition and implementation of associated logic are outside of `Michelson`.

## Untyped types

An untyped version of types is used to represent a contract after it is parsed.
It is capable of representing instructions and values with and without macros because it is parameterized by a type parameter for instructions.
This parameter can be an instruction with macros or without.

`U.Value` (a.k.a. `Michelson.Untyped.Value`) type is used to represent values as they are written in Michelson contracts.
It means that we don't have separate constructors for `address`, `key`, etc.
All these types are represented as strings.
The same applies to sets and lists, for example.

Untyped types are simple and easy to use, but they are not very powerful.
For instance, you can't statically ensure that all items in an untyped value storing a list have the same type.
Even if you check it, you will not be able to pass this guarantee to places where it matters thus still needing to handle impossible cases (via runtime errors).

## Typed types

A typed version of types is used to represent a contract after it is typechecked.
They actively use GHC extensions such as `GADTs` and `DataKinds`.
These types are more advanced than untyped types and are a bit harder to use.
However, they are much more powerful.

A typed Michelson value has type `T.Value t` (a.k.a. `Michelson.Typed.Value`) where `t` has kind `T` and `T` represents one of the Michelson types.
It allows statically knowing the Michelson type of each value.
As a consequence, we can ensure that a typed list always contains values of the same type.

A typed Michelson instruction has type `Instr (inp :: [T]) (out :: [T])` where `inp` and `out` are lists of Michelson types.
`inp` is input stack type, `out` is output stack type.
It allows us to specify the type of each Michelson instruction directly in Haskell.
This representation makes interpreter implementation much easier and less error-prone.
For instance, if you encounter the `CHECK_SIGNATURE` instruction, you statically know that current stack contains values of types `key`, `signature`, and `bytes`.
You don't need to check types dynamically, you can't confuse their order.

Typed representation doesn't support annotations.
Adding annotations to typed representation is quite a hard task, and its benefits are not worth doing it.
Annotations are apparently not needed for the interpreter.
They are checked by the typechecker and then stripped off.
