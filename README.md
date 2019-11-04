# Lorentz-contract-param

This package contains contracts written on Lorentz - Haskell eDSL for Michelson
contracts, including a Multisig Wrapper contract.

It also contains CLI interfaces for working with Lorentz contracts:
- `lorentz-contract`: list and export supported contracts to Michelson
- `lorentz-contract-param`: generate Michelson contract parameters
- `lorentz-contract-storage`: generate Michelson initial storage values

For more information, see the [quick start guide](https://assets.tqtezos.com/quickstart)
or specifically the [client setup guide](https://assets.tqtezos.com/quickstart/1-index).

Note: As of `09/27/2019` this package will not compile with the latest
`stack`: `2.1.3`.  To build, you'll likely need
[`stack v1.9.3`](https://github.com/commercialhaskell/stack/releases/tag/v1.9.3).

