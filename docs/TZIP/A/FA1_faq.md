---
tzip: FA1
title: Unsafe Ledger
status: WIP
type: Financial Application
author: John Burnham
advocate: John Burnham
created: 2019-04-12
---

## Why did you call this ledger the "Unsafe Ledger"?

Because it is unsafe. There are no protections against sending your balances to
nonexistent or inactive identities. If you deploy this and a user makes a
mistake, they can lose their entire balance. Do not use this unless you know
what you are doing and have extended it by adding appropriate safety features.

## Isn't that bad branding?

Yes, intentionally so. If you need a smart contract with friendly branding, please
use **FA1.1: Safe Ledger** (link is pending) instead.

This standard is intended to be used by experienced smart contract developers
internally in their applications and for no other purpose.
