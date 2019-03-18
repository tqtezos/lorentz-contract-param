# Morley NOP Instructions

Morley adds some additional instructions of its own to the existing set of
Michelson instructions. These instructions are not macros, and have no effect on
a contract's execution. That is, they cannot write or alter any value on the
stack, but simply provide a mechanism to pass additional
information to Morley's typechecker or runtime.

## `STACKTYPE`

The `STACKTYPE` instruction allows us to assert that the stack has a given type
signature. For example `STACKTYPE '[]` asserts that the stack is the empty
stack, and tells the typechecker to fail the contract otherwise.

### Stack Type Signature

A stack signature can be either an empty stack, a list of types, or a pattern
match on the head of the stack:

```
Empty stack: '[]
stack of three int: '[int, int, int]
A pattern match on a stack with two int at the top: '[int, int, ...]
```

More formally, a stack signature is like a `cons` list with two distinct
`nil`-like terminators:

```
<stack-sig> := "'[" (<empty-stack> | <rest-of-stack> | <stack-cons>)
<empty-stack> := "]"
<rest-of-stack> := "...]"
<stack-cons> := (<type> | "_" ) (("," (<stack-cons> | <stack-rest)) | <empty-stack>)
```

## PRINT

`PRINT` instructs Morley to print a comment during execution, optionally with
a reference into the stack. For example, `PRINT "hello"` will print `hello`.

Stack references may be written with in the print-comment as `%[n]`, replacing
`n` with any natural number, which prints the `n`-th stack element from the
head.

For example, `PRINT "Head is: %[0]"` on `'[1, ...]` will print

```
Head is: int 1
```

## TEST_ASSERT

The `TEST_ASSERT` instruction allows us to declare an inline test assertion, which is
a labeled sequence of instructions that runs on a copy of the current stack.
That is, an assertion has no actual effect on the program, but can return the
result of some computation on the stack.

For example, suppose we want to verify that the sum of two numbers is greater
than 10:

```
parameter unit;
storage unit;
code { DROP;
       PUSH int 2; 
       PUSH int 10;
       TEST_ASSERT Test1 "%[0] + %[1] > 10" {ADD; PUSH int 10; COMPARE;LT;};
       DROP; UNIT; NIL operation; PAIR; };

```

