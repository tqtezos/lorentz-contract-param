# Morley: An interpreter and developer library for the Michelson Language

## Goals

The future success of Tezos as a platform for smart-contracts depends on
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

In general, web IDEs are excellent tools for showcasing a new language when
the most important optimization criterion is absolute minimization of setup
friction. This is crucial when presenting a language to a non-technical
audience (for whom setup via a package manager might be distracting),
or to a technical audience that of undecided interest (for whom setup is too
large of a commitment). In other words, a web IDE is primarily an advertisement
and only secondarily (if at all) a practical production tool.

In practice, web IDEs are rarely used in the course of
developing production software. Several factors contribute to this, including
the greater performance overhead of web applications, the lack of features in
web IDEs compared to traditional editors (which often have had decades of
development, and millions of man-hours, poured into them), developer lock-in to
their preferred editor, etc, etc.

For the Michelson language, what is needed is a development tool to bridge the
gap between web-editor (easy setup, lacks features) and running a full Tezos
node (difficult setup, fully featured). The ideal solution is a lightweight
local runtime with minimal dependencies that can be set up and installed
quickly, so that developers can continue to use their comfortable local
configurations of e.g. editors and version control systems unimpeded.

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

The minimum viable project that achieves these goals consists of

1. A valid re-implementation of the Michelson generalized algebraic datatype as
   in the language spec. (Completed)
2. A parser that turns a `.tz` file into the above Michelson GADT (in-progress)
3. A typechecker that validates the parsed contract (in-progress)
3. A runtime for the Michelson stack machine that executes the parsed contract.
4. Testing features, such as generating arbitrary transactions or arbitrary
   valid contracts.
5. Running multiple contracts in parallel in the same sandbox.

Morley will use Haskell, which is very similar to OCaml in language features
(like GADTs), but also has exceptional libraries such as Megaparsec for parsing,
and QuickCheck for testing. Haskell also has excellent libraries for
parallelism, which makes it extremely interesting from a runtime perspective.
Using Haskell may also generate exposure for Michelson among a wider audience in
the function programming community.

## Why Haskell?

Haskell while not a particularly popular language in general, is an
extraordinarily popular language for building other languages, particularly for
building compilers. Languages whose reference compilers are implemented in
Haskell include:

- Elm
- Purescript
- Idris
- Agda

Compiler and interpreters have also been written for many other languages and
platforms, some of which are listed
[here](https://wiki.haskell.org/Applications_and_libraries/Compilers_and_interpreters#Generic_Haskell).

Furthermore, Haskell has historically been extremely successful at influencing
the design of other languages, even those not implemented in Haskell. Wikipedia
includes among its downstream influences:

- C++11
- C#
- Clojure
- CoffeeScript
- F#
- Python
- Rust
- Scala
- Swift

and others. The practical implication of this is that a large subset programmers
capable of significant contribution to a new programming language (which
Michelson undeniably is), are at least familiar with, if not expert in Haskell.

For Michelson to mature as a language, it needs to build a community of
engaged developers with language development experience. Building a Haskell
implementation of Michelson is one means of accomplishing this

Furthermore, it must be pointed out that Michelson is not just any programming
language, it is a smart-contract programming language. In this niche, Haskell is
also incredibly popular. Some examples of organizations that use Haskell to
develop smart contracts or blockchain infrastructure include:

- IOHK  (whose Cardano blockchain and the Plutus language are implemented in Haskell)
- Kadena (whose Chainweb blockchain and Pact smart-contract language is implemented in Haskell)
- Digital Asset (whose DAML smart-contract language is implemented in Haskell)
- Adjoint
- Serokell
- Symbiont

and others.

Tezos, which shares this niche of "functional programming + blockchain" could
greatly benefit from engaging with the significant and growing community of
blockchain Haskell developers.

Historically, the OCaml and Haskell communities have been largely distinct, but
this need not be the case going forward with Tezos. Both languages are built
upon similar principles of safety through static typing, type-inference,
parametric polymorphism etc. Cross-pollination between these two offshoots of
functional programming could be extremely fruitful.

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
