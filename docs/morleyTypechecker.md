# Morley Typechecker

In Morley, we have a typechecker for the core Michelson language, i. e. without macros.
It is located in `Michelson.TypeCheck` and designed in the following way:
* It takes a core Michelson contract extended with `EXT` instruction to support additional instructions described in the [`morleyInstructions.md`](./morleyInstructions.md) document.
* The contract passed to the typechecker uses [untyped representation](./michelsonTypes.md).
* During typechecking, we verify that instructions are well-typed and provide evidence to the compiler that allows us to convert instructions into [typed representation](./michelsonTypes.md). If the contract is ill-typed, the typechecking process fails with an error.
* The typechecker also takes the types of all of the contracts' parameters (needed for `SELF` and `CONTRACT` instructions) and a handler for the `EXT` instruction.

## CLI

End users can use the `typecheck` command to execute the typechecker.
It parses the contract, performs macro expansion, and passes it to the typechecker which says that the contract is well-typed or produces an error message.

Example: `morley typecheck --contract auction.tz`.

As a future improvement, we may implement verbose behavior of the reference implementation; for more details see [TM-84](https://issues.serokell.io/issue/TM-84).

## Limitations

The typechecker shouldn't be considered fully implemented and well-tested yet.
There are some known limitations of this typechecker which we are going to resolve later:
* [TM-59](https://issues.serokell.io/issue/TM-59). Some ill-typed (according to the reference implementation) contracts with `FAILED` instruction are considered valid by our typechecker.
* [TM-81](https://issues.serokell.io/issue/TM-81) Not all signing schemes are supported.

This list might be outdated, because we may discover new issues or fix existing ones.
You can see all the unresolved issues in our [issue tracker](https://issues.serokell.io/issues?q=project:%20%7BTezos%20Michelson%7D%20%23Unresolved).

## Internals

An internal structure of the typechecker is pretty well described in Haddock comments.
Two main modules are:
* `Michelson.TypeCheck.Value` contains logic for checking Michelson values.
* `Michelson.TypeCheck.Instr` contains logic for checking Michelson instructions and the whole contract.

Their functionality is re-exported from the `Michelson.TypeCheck` module along with some auxiliary types and functions.
