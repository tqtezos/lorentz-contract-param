---
tzip: FA1
title: Unsafe Ledger
status: WIP
type: Financial Application
author: John Burnham
advocate: John Burnham
created: 2019-04-12
---

## Summary

This document describes a smart contract that implements a ledger that maps
identities to balances. This ledger is described in a minimalistic way, and is
inteneded to be just a starting point or a single component of an application,
and not an application in its own right. To reemphasize: **This ledger is
unsafe. Do not use it without first adding safety features.**

Ideally, developers will prevent users from directly interacting with this
ledger at all and will layer additional interfacing contracts over it.

## Abstract

There is a need for a minimal abstract ledger that can be used as a component in
applications requiring some notion of fungible asset (or "token"). The space of
all possible contracts that require fungibility is vast, so this standard is
defined in the most general possible way.

Important possible features such as transfer approvals, fallback addresses or
monetary supply management are left to developers or extensions of this
standard.

## Unsafe Ledger Interface

```
parameter
  ( (address, nat)             %transfer
  | view unit nat              %getTotalSupply
  | view address (option nat)  %getBalance
  );
```

## Unsafe Ledger Storage

```
storage (big_map address nat, nat :total_supply);
```

The storage maintains a map of addresses to balances.

The `nat :total_supply` is to be supplied by the originator and must correctly
equal the sum of all balances in the `big_map` at origination. Any changes to
the sum of balances must be reflected by a corresponding changing the `nat
:total_supply`

## Entry-points

### transfer

This entry point will credit the account of the addresss passed in the
parameter, while debiting the account matching the contract source address.
Should the source address have insufficient funds, the transaction will fail and
no state will be mutated.

### getTotalSupply

This view simply returns the `nat :total_supply` in the storage.

### getBalance

This view will return `None` if the address is not in the ledger and
otherwise will return `Some x` where `x` is the corresponding balance.

## Variants

The above standard can be modified with the following variants. Implementations
which comply with one of these variants are still compliant with this standard
overall.

### Pruning Zero-balance accounts

A useful variant for efficiency is implement `transfer` such that accounts with
balances of `0` are removed from the ledger. This can reduce storage
costs in exchange possibly increasing the computational complexity of looking up
balances.

If this variant is implemented, the entry-point for `getBalance` should be
changed to

```
view address nat  %getBalance
```

Which should return `0` for any address not in the ledger.

### Generic Identities

All instance of `address` in the contract parameter and storage can be replaced
by `bytes`, which may be useful if a different notion of idenity is needed, such
as, for example, in implementing "Know your customer" (KYC) functionality.
