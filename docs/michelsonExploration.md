# Michelson Exploration

While we were familiarizing with Michelson we were making some experiments to better understand how it works.
Here we present results of these experiments.


## `contract` type

Michelson has `contract 'param` data type.
Here are some facts about it.

It's possible to pass a contract from CLI: you should pass contract's address to do it.

`tz1` address can also represent a contract.
E. g. `"tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"` is a valid value of `contract unit` type.
Such a contract can be returned by the `IMPLICIT_ACCOUNT` instruction.
It doesn't have any code.

Apparently `contract 'param` type can represent only a contract that has been already originated.
A simple script was used to check it:

```
parameter string;
storage (contract string);
code {
      CDR;
      NIL operation; PAIR;};
```

It fails on storage `KT1TCsKEoi37Y5tVQy4dhCJRUcpBhUYMCcXa` (not originated on alphanet) and succeeds on storage `KT1TDFxATmqqTcPWPJvrv7XC6EDS82ztRntf` (originated on alphanet).

`UNPACK` operation also does this check.
The following scripts was used to check it:

```
parameter string;
storage bytes;
code {
      CDR;
      UNPACK (contract string);
      IF_NONE { PUSH bytes 0x00 }
              { DROP; PUSH bytes 0x01 };
      NIL operation; PAIR;};
```

It was launched on storage value `0x050a0000001601cc58ef2fd58f967eaf9d5676bed4509f2523ae8c00` using alphanet and zeronet scripts and returned different values (0x01 on alphanet and 0x00 on zeronet).
It's because this storage value is the result of `PACK` applied to the `KT1TDFxATmqqTcPWPJvrv7XC6EDS82ztRntf` contract.

One more operation that can put a contract on stack is `IMPLICIT_ACCOUNT`.
Implicit account always exists, so it always puts an existing account on stack.

## Checking string literal of a contract/address

As was written above, contract's string literal is the same as address' string literal.
It can even be a `tz1` address in which case it has type `contract unit`.
However, as we know, `contract` type can represent only a contract that has been originated.
Hence a question arises: when should we check whether a string literal corresponding to a contract is valid (i. e. the contract itself is originated).
The following script has been used to test it:

```
parameter string;
storage string;
code { CDR;
       PUSH (contract string) "KT1TDFxATmqqTcPWPJvrv7XC6EDS82ztRntf";
       DROP;
       NIL operation; PAIR;};
```

This script is well-typed and doesn't fail in runtime if we use alphanet.
* If we modify an arbitrary letter in the pushed string literal, the script will be ill-typed, because the string will be invalid base58check string. In this case it doesn't matter whether we push `contract string` or `address`, it will be ill-typed in both cases.
* If we change the pushed type to `contract key_hash`, the script will be ill-typed (such contract exists, but has a different parameter type).
* If we change the pushed type and value to `PUSH (contract (or :_entries (unit %_Liq_entry_open) (unit %_Liq_entry_join))) "KT19iqoRhBwGdgNVQn6mnXLtoY3bige35CVq";`, the script will be ill-typed on alphanet and well-typed on zeronet (it's a valid script from zeronet).
* And if we use alphanet version and try to push an `address` from zeronet, the script will be well-typed.

Summary of how typechecker checks contract/address string literals:
* Base58check format is always checked.
* If the type is `address`, nothing else is checked (even if it's a `KT1`-address of a non-existing contract, it's fine).
* If the type is `contract t`, type checker also checks that there is a contract with given parameter type and address that has been originated in the network.

It means that such typechecker should have access to all originated contracts.
It's not necessary to access their code, it's enough to know all addresses and corresponding parameter types.

## Computation of contract's address

Specific format is not yet clear, but according to [this answer](https://tezos.stackexchange.com/a/361/342) it's a pure function which takes origination command as input.
So contract itself is not enough to compute its address (obviously there can more than one contract with the same code), we also need to know its initial value, manager, etc.

## `client run script` command

`client run script` command is a bit obscure, because normally in order to run a contract you need to originate it first and then send a transaction to it.
This command seems to do both operations at once. We don't specify its environment (e. g. contract's balance), but it works nonetheless.
Probably it uses some hardcoded values.
I ran a contract which returns its own address.
Then I passed this address to a script whose storage has contract type.
It was passed successfully, even though the contract hasn't been actually originated on alphanet (I checked it in block explorer).
So apparently it was stored somewhere locally.
I don't know how exactly it works.

## On contract's parameter type

Contract's parameter type can be easily tricked and implicitly casted when making a call (making unexpected annotation mismatch)

Let's originate contract `test1.tz`:

```
parameter (pair (int :t) (int %s));
storage int;

code { DUP; DUP;
       CAAR;
       DIP { CADR; };
       ADD;
       DIP { CDR; };
       ADD;
       NIL operation;
       PAIR;
     }
```

```
./alphanet.sh client originate contract test1 for alice transferring 1 from alice running container:test1.tz --init 0 --burn-cap 1
```

Say, it was originated with identifier `KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9`.
Then let's originate another contract `test2.tz`:

```
parameter (pair (int :p) (int %q));
storage unit;

code {
       PUSH (contract (pair int int)) "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9";
       SWAP;
       PUSH mutez 10000;
       SWAP;
       CAR;
       TRANSFER_TOKENS;
       NIL operation;
       SWAP; CONS;
       UNIT;
       SWAP;
       PAIR;
     }
```

```
./alphanet.sh client originate contract test2 for alice transferring 1 from alice running container:test2.tz --init Unit --burn-cap 1
```

And finally call it:

```
./alphanet.sh client transfer 0 from alice to test2 --arg 'Pair 11 23'
```

Everything works like a charm.
We effectively launched contract with parameter type `pair (int :t) (int %s)` passing `pair (int :p) (int %q)` as parameter to it.

Just for the reference, slight modification to `test2.tz` leads to type check error:

```diff
parameter (pair (int :p) (int %q));
storage unit;

code {
-       PUSH (contract (pair int int)) "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9";
+       PUSH (contract (pair (int :p) (int %q))) "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9";
       SWAP;
       PUSH mutez 10000;
       SWAP;
       CAR;
       TRANSFER_TOKENS;
       NIL operation;
       SWAP; CONS;
       UNIT;
       SWAP;
       PAIR;
     }
```
