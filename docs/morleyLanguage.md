# Morley Language

The Morley Language is a low-level syntactic sugar over the core Michelson
instructions (core Michelson being the instructions which are actually executed
in the Tezos blockchain, as opposed to the various syntactic conveniences
provided by the OCaml reference client)

The general principle is that any syntactically valid core Michelson expression
will also be a valid Morley expression, i.e. Morley is a superset of
Michelson. Any language extensions that break this principle must be explicitly
enabled

## Pairs

### Type Syntax
`pair` types may be written using Haskell-style tuples:

```
(a, b) ~ (pair a b)
(a, b) :t %f ~ (pair :t %f a b)
```

When tuples are nested, parenthesis may be omitted:

```
(a,b,c) ~ (a,(b,c))
```

But if so, only the outer pair may be annotated:

```
(a, b, c) :t %f ~ (a, (b, c)) :t %f
```

Inner types may be annotated as usual:

```
(a :ta %fa, b :tb %fb, c :tc %fc) ~ (a :ta %fa, (b :tb %fb, c :tc %fc))
```

### Value Syntax

`Pair` values may also be written with tuples:

```
(a, b) ~ (Pair a b)
(a, b, c) ~ (a,(b,c))
```

## Unions

### Type Syntax

`or` types may be written using the `|` character: 

```
(or a b) ~ (a | b)
(or :t %f a b) ~ (a | b) :t %f
```

When bars are nested, parenthesis may be omitted:

```
(a | b | c) ~ (a | (b | c))
```

Annotations follow the same pattern as Tuples:

```
(a | b | c) :t %f ~ (a | (b | c)) :t %f
(a :ta %fa | b :tb %fb | c :tc %fc) ~ (a :ta %fa | (b :tb %fb | c :tc %fc))
```

## Unit

The `unit` type may be written as a `0`-tuple

```
unit ~ ()
```

The `Unit` value may also be written this way:

```
Unit ~ ()
```

## Lambda: 

They `lambda` type may be written:

```
(lambda a b) ~ (\ a -> b)
```

## Containers

The `list` and `set` types may be written:

```
(list a) ~ [a]
(set a) ~ {a}
```

## Instruction syntax:

Instructions and macros may be written in lower case:

```
DROP ~ drop
```

## Custom Macros

TBD

## Inline Testing

TBD
