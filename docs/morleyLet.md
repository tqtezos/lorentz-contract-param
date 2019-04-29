# Let-block Definitions

In addition to the built-in macros defined in the Michelson specification,
Morley allows the programmer to define their own custom macros, as well as value
and type synonyms in a new top-level block:

```
let {
  type num = int;
  Three :: int = 3;
  add3 :: '[int] -> '[int] = {push int 3; add;};
  add3 :: '[int] -> '[int] = {push num Three; add;};
  add :: '[int, int] -> '[int] = {sub;};
  stkRest :: '[int, ...] -> '[int, ...] = {push int 3; ADD;};
};

parameter unit;
storage unit;
code { DROP;
       PUSH nat Three;
       drop;
       PUSH num Three;
       push Three;
       add;
       stacktype '[int, ...];
       stkRest;
       DROP;
       UNIT; NIL operation; PAIR; };
```

More formally:

```
let {<let>};

<let> := type <string> = type;
    | <string> :: <type> = <value>;
    | <string> :: <type-sig> = {op};
```

As a concrete example:

```
let {
  type num = int;
  Three :: int = 3;
  add3 :: '[int] -> '[int] = {push int 3; add;};
};
```

There are three distinct types of let:

- Type synonyms
- Constants
- Macros

## Constants

Constants may be pushed directly to the stack like so:

```
let {
  Three :: int = 3;
};
...
code {
  ...
  push Three;
  ...
  }
```

At present, constants are not checked statically, only when pushed.

## Type Synonyms

Type synonyms allow for labeling types. A synonym can be referred to anywhere a
type would be expected:

```
let {
  type num = int;
}
code {
  ...
  push num 3;
  ...
  }
```

This `push num 3` instruction pushes the value 3 with the type `int :num` onto
the stack.

Note that type synonyms with names `Parameter` and `Storage` are prohibited, these
names are used for implicit contracts parameter and storage types.

The interaction between type synonyms and type annotations is that if the
synonym's definition contains no annotation, the name of the synonym will be
used. If the synonym definition does contain a type annotation, the annotation
takes precedence and will be preserved:

```
type color = int;     ~> int :color
type color = int :rgb ~> int :rgb
```

## Let Macros

```
add3 :: '[int] -> '[int] = {push int 3; add;};
```

The first line of the macro declaration is the type signature, which denotes the
stack transformation the macro performs.

Crucially, if both input and output stack types contain a `<rest-of-stack>`
pattern match (syntactically `...]`), then the stack type captured by both
pattern matches must be identical.

For example, the type signature of 

```
add3 :: '[int, ...] -> '[int, ...] = {push int 3; add;}
```

would be written using the type notation from the Michelson specification as:

```
add3 :: int : 'S -> int : 'S
```

meaning that the pattern match must be universally quantified over the same
stack-type `'S`. 

Furthermore, type signatures can also have universally quantified type
parameters, which must be declared in a `forall`:

For instance, the type of the primitive `SWAP` instruction could be notated as:

```
swap :: forall a b. '[a, b, ...] -> '[b, a, ...]
```

It is important to note that type variables are not checked statically (at
present), but rather concretized in-place during type-checking. That is, the
typechecker replaces any unbound type parameter with the concrete type at that
position in the stack, and checks for consistency with any other instances of
that parameter.

## Non-recursive References

These definitions allow for non-recursive references to other definitions,
so one can define:

```
let {
  foo :: '[unit] -> '[unit, unit] = {unit;};
  bar :: '[unit] -> '[unit] = {foo; drop;};
};
```

A reference must always come after the let it refers to in the block.


