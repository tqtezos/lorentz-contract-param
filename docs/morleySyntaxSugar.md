# Morley Language: Syntax Sugar

## Summary

| type   | Sugar                                   | Desugar                                   |
|--------|-----------------------------------------|-------------------------------------------|
| pair   | `(a ,b)`                                | `(pair a b)`                              |
| pair   | `(a, b) :t %f`                          | `(pair :t %f a b)`                        |
| pair   | `(a,b,c)`                               | `(a,(b,c))`                               |
| pair   | `(a, b, c) :t %f`                       | `(a, (b, c)) :t %f`                       |
| pair   | `(a :ta %fa, b :tb %fb, c :tc %fc)`     | `(a :ta %fa, (b :tb %fb, c :tc %fc))`     |
| or     | `(a \| b)`                              | `(or a b)`                                |
| or     | `(a \| b) :t %f`                        | `(or :t %f a b)`                          |
| or     | `(a \| b \| c)`                         | `(a \| (b \| c))`                         |
| or     | `(a \| b \| c) :t %f`                   | `(a \| (b \| c)) :t %f`                   |
| or     | `(a :ta %fa \| b :tb %fb \| c :tc %fc)` | `(a :ta %fa \| (b :tb %fb \| c :tc %fc))` |
| unit   | `()`                                    | `unit`                                    |
| lambda | `(\ a -> b)`                            | `lambda a b`                              |
| list   | `[a]`                                   | `list a`                                  |
| set    | `{a}`                                   | `set a`                                   |



| Value | Sugar       | Desugar      |
|-------|-------------|--------------|
| Pair  | `(a, b)`    | `(Pair a b)` |
| Pair  | `(a, b, c)` | `(a,(b,c))`  |
| Unit  | `()`        | `Unit`       |

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

