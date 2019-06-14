---
tzip: FA1.4
title: Managed ERC-20 Ledger
status: WIP
type: Financial Application
author: John Burnham, Konstantin Ivanov
advocate: John Burnham, Konstantin Ivanov
created: 2019-05-24
---

## Summary

This document describes a smart contract which implements a ledger that maps
identities to balances. This ledger implements ERC-20 interface, and
additionally provides `Mint`, `Burn`, `Approve` and `Pause` operations.

## Managed ERC-20 Ledger Interface

```
parameter
  ( (address :from, address :to, nat :val) %transfer
  | (address :to, nat :val)                %approve
  | bool                                   %setPause
  | address                                %setManager
  | view unit manager                      %getManager
  | view (address :from, address :to) nat  %getAllowance
  | view unit nat                          %getTotalSupply
  | view address nat                       %getBalance
  );
```

See also [syntax explanation](https://gitlab.com/morley-framework/morley/blob/master/docs/morleySyntaxSugar.md) and [Michelson Contract Interfaces and Conventions document](https://gitlab.com/tzip/tzip/blob/master/A/A1.md#view-entry-points).

## Managed ERC-20 Ledger Storage

```
storage
  ( big_map address (nat :balance, map address nat :approvals)
  , address :manager
  , bool :paused
  , nat :total_supply
  );
```

The storage maintains a map of addresses to a pair.
The first element of this pair is address balance, and the second is a map
keeping number of approvals for each contract which has requested withdrawal
approvals from the given address.

The `address :manager` is set by the originator and refers to a contract
with exclusive right to pause all operations and seize funds. This address
also keeps (virtually) all funds which are withdrawn with `Mint` operation
and are deposited with `Burn` operation. We never store manager address in
balance or approvals maps and assume that manager has infinite funds.

Value `bool :paused`, set to `True`, prevents all operations
involving money transfer or allowance changes.

The `nat :total_supply` is to be supplied by the originator and must correctly
equal the sum of all balances in the `big_map` at origination. Any changes to
the sum of balances must be reflected by a corresponding changing the `nat
:total_supply`

## Errors

Failures of this contract are represented as
`(string, d)` pairs, the first element of which
is an identifier of an error and the second element keeps details of this error,
or `unit` if no details required.

For example, attempt to withdraw `5` tokens when only `3` is present
will result in the following error:
`("NotEnoughBalance", (5, 3))`

## Entry-points

### transfer

This entry point will credit the account of the address passed in the
parameter, while debiting the account matching the contract source address.
Should the source address have insufficient funds, the transaction will fail and
no state will be mutated.

We move away from ERC interface marginally and have only one `transfer` entry point.
When called with source account equal to the address of contract which initiates
the current transaction, then it is `transfer` from ERC-20, otherwise we resort
to the logic of `transferFrom`.

`Mint` and `Burn` operations are just corner cases of this entry point with
source account or destination account set to manager address correspondingly.

Manager is priveleged to transfer money between any two accounts. Other accounts
can spend only their own funds and funds they were authorized to spend by
other accounts.

This entry point can fail with the following errors:
* `NotEnoughBalance` - insufficient funds on source account to perform given
transfer. The error will contain a `(nat :required, nat :present)` pair, where
`required` is requested amount of tokens, `present` is available amount.
* `NotEnoughAllowance` - given account has no permission to withdraw given
amount of funds. The error will contain a `(nat :required, nat :present)` pair,
where `required` is requested amount of tokens, `present` is current allowance.
* `OperationsArePaused` - operations with token has been paused.

### approve

This entry point, called with `(address: to, nat: val)`
parameters allow `to` account to withdrawal from initiator, multiple times,
up to the `val` amount. Each call of `transfer` entry point decreases
allowance amount in the storage on transferred amount of tokens.

If this entry point is called again, it overwrites the current allowance
with `val`.

Changing approval value from non-zero value to a non-zero value is
forbidden to prevent the [corresponding attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM).

This entry point can fail with the following errors:
* `UnsafeAllowanceChange` - attempt to change approval value from non-zero to
non-zero was performed. The error will contain `nat :previous` value, where
`previous` stands for the allowance value upon contract call.
* `OperationsArePaused` - operations with token has been paused.

### setPause

This entry point pauses operations when the parameter is `True`,
and resumes them when the parameter is `False`. During pause,
no contract including the token manager can perform `transfer` or
`approval` operations.

This entry point can fail with the following errors:
* `InitiatorIsNotManager` - caller is not the token manager.

### setManager

Change current manager.

This entry point can fail with the following errors:
* `InitiatorIsNotManager` - caller is not the token manager.
* `ManagerAddressWouldShadow` - cannot set token manager to the given address
because it already appears in the ledger.

### getManager

This view returns the current manager.

### getTotalSupply

This view simply returns the `nat :total_supply` in the storage.

### getBalance

This view will return balance of the address in the ledger, or
`0` if no such address is stored.
