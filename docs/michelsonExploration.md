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
