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
