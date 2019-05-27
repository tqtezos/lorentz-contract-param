# Lorentz-contracts

This package contains contracts written on Lorentz - Haskell eDSL for Michelson
contracts.
They include:

* Contracts demonstrating advanced Lorentz features, for instance
[walker contract](src/Lorentz/Contracts/Walker.hs).
* Examples of applied contracts, for instance
[auction contract](src/Lorentz/Contracts/Auction.hs)
* Securitize DS Token [^1] implementation - _work in progress_.

[^1]: https://www.securitize.io/

## Contract registry

To read contracts defined in this package one can use `lorentz-contracts` executable.

Example:
```sh
cd lorentz-contracts && make lorentz-contracts
stack exec lorentz-contracts -- list
```
Shows all registered contracts.

```sh
stack exec lorentz-contracts -- print auction
```
Prints the code of auction contract.

### Contracts discovery

Contracts defined in this package are registered automatically.
Define your contracts following the `contract_<name>` pattern. Example:
```hs
module Lorentz.Contracts.FungibleToken
  ( contract_Fungible_token
  ) where

...
```
This way, `Fungible token` contract will be available in the registry.

Automatic discovery is implemented via `contract-discovery` executable.
Being called via the GHC preprocessor from within a Haskell module,
it scans all modules within the same directory and forms a contract map which can
be later imported. See [`app/LorentzContracts.hs`](app/LorentzContracts.hs) for example.
