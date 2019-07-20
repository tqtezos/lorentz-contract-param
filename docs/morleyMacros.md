# Morley Language: Macros and Type synonyms

Morley language supports all macros from the OCaml reference implementation and additionally introduces some extra macros and type synonyms.

## Implicit Parameter and Storage

Morley language supports implicit types `Parameter` and `Storage` in contracts code and let-block macros.
As an example, this contract:
```
let {
  add5 :: '[Parameter] -> '[Parameter] = {push Parameter 5; add};
};

parameter int;
storage unit;
code {car;
      add5;
      drop;
      push Storage Unit;
      nil operation;
      pair;
}
```
Will be equivalent to:
```
let {
  add5 :: '[int] -> '[int] = {push int 5; add};
};

parameter int;
storage unit;
code {car;
      add5;
      drop;
      push unit Unit;
      nil operation;
      pair;
}
```


## A1 macros

First group of macros implemented in Morley consists of macros proposed in [A1 TZIP](https://gitlab.com/tzip/tzip/blob/master/A/A1.md).
In order to avoid duplication of documentation we don't copy it here, please refer to the A1 document for more information.

## Macros to work with tuples

Large tuples are desugared as right-balanced trees of `Pair`, thus working with them manually is inconvenient and produces fragile code.
The following macros can be used to work with tuples without caring about their actual Michelson representation.

### ACCESS

This macro retrieves an element of a tuple.
It accepts two arguments - 0-based index of desired element and overall tuple size.
For now `ACCESS` is just a macro, not a full-featured morley instruction, thus programmer has to specify tuple size explicitly.

Example:
```
push (int, bool, string, unit) (1, False, "", ());
access 1 4;
# Now stack contains `False` at the top
```

### SET

Modifies single element of a tuple.
This macro has signature similar to one of `ACCESS`, and accepts two stack arguments - original tuple and new value.

Example:
```
push (int, bool, string, unit) (1, False, "", ());
push bool True;
set 1 4;
# Now stack contains `(1, True, "", ())` at the top
```

### CONSTRUCT

Builds a tuple.
For each position of the tuple you provide a code block which builds an element for that position.

Example:
```
construct
  { push int 1 }
  { sender }
  { unit; some }
# Pushes `(1, <sender address>, some ())` on stack
```

Each code block has access to the stack at the moment of `construct` call and eventually have to push exactly one element to stack. This will also work:
```
push 5
construct
  { dup }
  { push int 3 }
  { dup; push int 3; add }
# Now stack is [(5, 3, 8), 5, ...]
```
