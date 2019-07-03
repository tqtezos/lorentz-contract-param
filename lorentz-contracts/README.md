# Lorentz-contracts

This package contains contracts written on Lorentz - Haskell eDSL for Michelson
contracts.
They include:

* Contracts demonstrating advanced Lorentz features, for instance
[walker contract](src/Lorentz/Contracts/Walker.hs).
* Examples of applied contracts, for instance
[auction contract](src/Lorentz/Contracts/Auction.hs)

## Contract registry

To read contracts defined in this package one can use `lorentz-contracts` executable.

Example:
```sh
cd lorentz-contracts && make lorentz-contracts
stack exec lorentz-contracts -- list
```
Shows all registered contracts.

```sh
stack exec lorentz-contracts -- print -n auction
```
Prints the code of auction contract.

All contracts should appear in the registry; for this, add your contract to [the executable](./app/Main.hs).
