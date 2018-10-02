# Morley: An interpreter and developer library for the Michelson Language

## Goals

The future success of Tezos as a platform for smart-contracts depends on the
the growth of the Michelson developer community. The best way to encourage that
growth is to build excellent documentation and tools to streamline the developer
experience.

Two significant pain-points for a developer learning how to program in Michelson
are: 1. The difficulty in setting up a development environment and 2. The lack
of comprehensive and up-to-date educational and tutorial content.

These two challenges can be addressed by building a standalone Michelson
interpreter that is separate from the full Tezos OCaml implementation. Michelson
currently requires the developer to build and run a Tezos node, which takes
non-trivial time and effort. Consequently, the barrier to a potential developer
simply playing with and testing out the language is relatively high, unless they
are exceptionally motivated to set up Tezos infrastructure. Services like [Try
Michelson](https://try-michelson.com/) partially solve this problem by wrapping
a web application around a Tezos node running a sandbox, but the solution
is not ideal from a developer experience perspective due to the limitations of
web UI's for code editing.

The ideal solution is a lightweight local runtime with minimal dependencies
that can be set up and installed quickly, so that developers can continue to
use their comfortable local configurations of e.g. editors and version control
systems unimpeded.

Another beneficial effect of a standalone Michelson runtime is in providing
clarity to the Michelson documentation. The existence of multiple interpreters,
especially when implemented with different languages allows us to better pinpoint
ambiguities, or other possible areas of improvement in the specification. A
specification ought to be language agnostic, and the best way to discover
whether there are important details in the OCaml reference implementation not
captured in the specification is to write an implementation in a different
language.

Furthermore, as the Michelson specification has been rapidly evolving over the
past several months, much of the existing documentation is obsolete or
incomplete. Implementing a standalone interpreter will help clarify what the
present state of the language in the reference implementation truly is, since
any inconsistencies can be detected through testing. Specifically, a standalone
interpreter can be reliably kept up to date with the reference implementation by
running an appropriately designed test suite (perhaps with some continuous
integration). If under testing the standalone implementation deviates from the
reference implementation, such as after a language update, for example, then
that difference in behaviour becomes a valuable opportunity for improving the
Michelson language documentation.

## Morley 1.0: a minimalist Michelson interpreter in Haskell

The minimum viable project that acheives these goals consists of

1. A valid re-implementation of the Michelson generalized algebraic datatype as
   in the language spec.
2. A parser that turns a `.tz` file into the above Michelson GADT
3. A typechecker that validates the parsed contract
3. A runtime for the Michelson stack machine that executes the parsed contract.
4. Testing features, such as generating arbitrary transactions or arbitrary
   valid contracts.
5. Running multiple contracts in parallel in the same sandbox.

Morley will use Haskell, which is very similar to OCaml in language features
(like GADTs), but also has exceptional libraries for building language tools,
such as Megaparsec for parsing, QuickCheck for testing. Haskell also has
excellent libraries for parallelism, which makes it extremely interesting from a
runtime perspective. Using Haskell may also generate exposure for Michelson
among a wider audience in the function programming community.

## Future work

The components required for a Michelson runtime substantially overlap
with those required for other useful tools. Some examples:

- Michelson bindings in the interpreter's implementation language
- Static analysis of Michelson code and control flow visualisation
  (a la [ethereum-analyzer](https://github.com/zchn/ethereum-analyzer))
- Testing tools, especially property testing harnesses and random datatype
  generators.
- Tools for high-level languages like Liquidity, or our own new language.
- Trans-compilers from other smart-contract languages such as Pact or Solidity.

The name "Morley" for this project comes from the Michelson-Morley experiment
which investigated the properties of the then-dominant theory of luminiferous
aether, and was the inspiration for Einstein's theory of special Relativity.
Should this work evolve into our own new high-level language, Relativity would
be a good name. Perhaps "Special Relativity" for a non-Turing complete subset,
and "General Relativity" for the full language.
