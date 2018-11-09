# Morley: Developer tools for the Michelson Language

Morley is a library to make writing smart contracts in Michelson pleasant and
effective.

1. A reimplementation of the Michelson Language in Haskell

  a. An abstract data type representing Michelson smart contracts and expresions
     (Language.Michelson.Types)
  b. A parser to turn a `.tz` file into that ADT (Language.Michelson.Parser)
  c. A typechecker that validates ADT's that conform to Michelson's typing
  rules (Language.Michelson.Typecheck)
  d. An interpreter that executes a well-typed Michelson smart contract in a
  sandbox (Language.Michelson.Runtime)

2. *Speculative*: Testing tools. Some ideas:
  a. A REPL with a good stack visualizer (and maybe we can add breakpoints with
  Michelson's annotations?)
  b. A QuickChek harness and generators (What are the interfaces a smart
  contract exposes to the world, and what happens when we Monte-Carlo the heck
  out of them?)
  c. Simulating a more realistic network environment, multiple smart contracts
    in the same sandbox.

3. *Speculative*: Morley syntax, a low-level syntactic sugar for Michelson:
  a. Michelson's instructions are typed with all capital letters which is
  a little inconvenient. I don't think capitalization is actually necessary to
  disambiguate types (lower case), data tags (Mixed case), and instructions
  (UPPER CASE). I think we can make instructions lower case and use context to
  distinguish them from types.
  b. The pair type constructor adds noise to type signatures. I think we can
  just make this implicit with `(pair a b) => (a, b)`
  c. Reduce paren noise with `(a, (b, c)) => (a, b, c)`
  d. The CAR/CDR macros aren't great. I think we can just treat
  `(a, b, c, d)` as a "list"-like and use an index macro. `index n => CA{D*n}R`
  e. The PAIR macro is also not great, maybe the family of `(,)`, `(,,)` etc.

4. Morley might make a good target for a transpiler like Juvix.

