# Morley: Developer tools for the Michelson Language

Morley is a library to make writing smart contracts in Michelson pleasant and
effective.

## I: A reimplementation of the Michelson Language in Haskell

1. An abstract data type representing Michelson smart contracts and expresions
   (see `Language.Michelson.Types`)
2. A parser to turn a `.tz` file into that ADT (see `Language.Michelson.Parser`)
3. A typechecker that validates ADT's that conform to Michelson's typing rules
   (see `Language.Michelson.Typecheck`)
4. An interpreter that executes a well-typed Michelson smart contract in a
   sandbox (see `Language.Michelson.Runtime`)

## II: Testing tools (*Speculative*)

1. A REPL with a good stack visualizer (and maybe we can add breakpoints with
   Michelson's annotations?)
2. A QuickChek harness and generators (What are the interfaces a smart contract
   exposes to the world, and what happens when we Monte-Carlo the heck out of
   them?)
3. Simulating a more realistic network environment, multiple smart contracts in
   the same sandbox.

## III: Morley syntax, a low-level syntactic sugar for Michelson (*Speculative*)

1. Michelson's instructions are typed with all capital letters which is a little
   inconvenient. I don't think capitalization is actually necessary to
   disambiguate types (lower case), data tags (Mixed case), and instructions
   (UPPER CASE). I think we can make instructions lower case and use context to
   distinguish them from types.
2. The pair type constructor adds noise to type signatures. I think we can just
   make this implicit with `(pair a b) => (a, b)`
3. Reduce paren noise with `(a, (b, c)) => (a, b, c)`
4. The CAR/CDR macros aren't great. I think we can just treat `(a, b, c, d)` as
   a "list"-like and use an index macro. `index n => CA{D*n}R`
5. The PAIR macro is also not great, maybe the family of `(,)`, `(,,)` etc.

## IV: Transcompilation (*Super-speculative*)

1. Morley might make a good transpiler target for a library like Juvix (GHC Core
   to Michelson transpiler)

2. I feel like nominative determinism might allow me to stretch this library to
   include an EVM interpreter and Michelson to EVM transpiler too
   (Michelson-Morley disproved the theory of luminiferous aether)


