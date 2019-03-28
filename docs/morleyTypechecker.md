# Morley Typechecker

In Morley, we have a typechecker for the core Michelson language, i. e. without macros.
It is located in `Michelson.TypeCheck` and designed in the following way:
* It takes a core Michelson contract extended with `EXT` instruction to support additional instructions described in the [`morleyInstructions.md`](./morleyInstructions.md) document.
* The contract passed to the typecheck uses [untyped representation](./michelsonTypes.md).
* During typechecking we check that instructions are well-typed and provide an evidence to the compiler that allows us to convert instructions into [typed representation](./michelsonTypes.md). Or fail with an error if the contract is ill-typed.
* Typechecker also takes types of all contracts' parameters (needed for `SELF` and `CONTRACT` instructions) and a handler for the `EXT` instruction.

## CLI

End users can use the `typecheck` command to execute the typechecker.
It parses the contract, performs macro expansion and passes it to the typechecker which says that the contract is well-typed or produces an error message.

Example: `morley typecheck --contract auction.tz`.

As a future improvement we may implement verbose behavior of the reference implementation, see [TM-84](https://issues.serokell.io/issue/TM-84).

## Limitations

The typechecker shouldn't be considered fully implemented and well-tested yet.
There are some known limitations of this typechecker which are going to resolve later:
* [TM-59](https://issues.serokell.io/issue/TM-59). Some ill-typed (according to the reference implementation) contracts with `FAILED` instruction are considered valid by our typechecker.
* [TM-81](https://issues.serokell.io/issue/TM-81) Not all signing schemes are supported.

This list may be outdated, because we may find discover new issues or fix existing ones.
You can see all unresolved issues in our [issue tracker](https://issues.serokell.io/issues?q=project:%20%7BTezos%20Michelson%7D%20%23Unresolved).

## Internals

Internal structure of the typechecker is pretty well described in Haddock comments.
The main two modules are:
* `Michelson.TypeCheck.Value` contains logic for checking Michelson values.
* `Michelson.TypeCheck.Instr` contains logic for checking Michelson instructions and whole contract.

Their functionality is re-exported from the `Michelson.TypeCheck` module along with some auxiliary types and functions.
