## Introduction
During the past several years, bugs and vulnerabilities in smart contracts caused millions of dollars to get stolen or lost forever. Such cases may even require [manual intervention][eth-dao] in blockchain operation to recover the funds. Apart from improving tools and languages, the community starts to acknowledge the need for upgradeable smart-contracts.

As the contracts get more and more complicated and integrated with the infrastructure, deploying a new version that is independent from the previous one is no longer an option. There are several goals one would usually like to achieve while developing an upgradeable contract:
  1. **Address immutability.** The contract address is used in users’ wallets, exchange integration code, external DApps, etc. Forcing users to change the contract address is quite hard to achieve. Moreover, variable contract address makes phishing attacks more probable. This is why it is desirable to leave the contract address intact while upgrading the contract.
  1. **Storage migration.** The new version of the contract must somehow know about the state of the previous one. As we show later in this document, this requirement is easy to fulfill for contracts with several bytes of storage but it becomes non-trivial when the storage size grows.
  1. **Type safety.** Since smart contracts are usually used to handle value, a cost of mistake can be large. Compile-time type safety may facilitate catching common programmer mistakes before the contract is deployed. This, in turn, leads to safer contracts and less value lost or stolen.

There are platforms that support upgradeability on the protocol level. For example, EOS ties contracts to the originator accounts that are able to upgrade their contracts at any point without any restrictions. It may give rise to fraudulent contracts pretending to be innocent at the time of origination but starting to behave malevolently after an upgrade. EOS community realizes this problem exists and takes some [precautions][eos-precautions] limiting the probability of such an outcome. Another example is Neo – a blockchain platform that also [offers][neo-migrate] smart contract migrating functionality. Unlike EOS, Neo performs only storage migration, while the address of the new version does not remain the same.

Other platforms like Ethereum or Tezos neither allow the contract code section to be modified, nor provide ways to deploy a new contract with an old storage. In order to make upgrades possible on these platforms, contract developers have to be explicit about their intent.

The contract code in Ethereum is immutable by design. The [only way][eth-yp] to execute a piece of code not supplied during the contract creation in EVM is to create a new contract and perform one of the `CALL`, `CALLCODE` or `DELEGATECALL` instructions on it (the difference between these is nicely described on [Ethereum StackExchange][eth-call-delegatecall]). The most important fact for the sake of this document is that the `DELEGATECALL` instruction preserves the execution context, i.e. the contract storage, transaction sender and the value remain as if the code of the callee was copy-pasted to the caller contract. This instruction as well as the described EVM limitations on custom code execution are the reasons why [_delegate proxy_ pattern][eth-proxy] has gained popularity as a simple way to create a contract with an upgradeable implementation, and the constant address and storage.

Contrary to Ethereum, Michelson can store and execute user-supplied lambda functions from storage. On the other hand, unlike `DELEGATECALL`, Michelson operations do not preserve the execution context. Thus, applying the proxy pattern to smart-contracts in Michelson requires the contract author to carefully pass the original sender and value as parameters to the callee. Moreover, if a new version of a contract is originated, the storage must be fully or partially transferred to the new version as well.

There are three most common options for storage upgrades — manual upgrade, [eternal storage][eth-eternal] and [lazy upgrades][eth-lazy].

When one originates a new contract in Tezos, she supplies an initial storage value. If the storage has just several entries, one can get the storage of the old contract and push it to the new one. We call this a **manual upgrade**. However, this approach is limited to small contracts only. There is an intrinsic limit on `max_operation_size` that makes cloning the storage cumbersome when it comes to copying `BigMap`s, large lists and other big chunks of data. Theoretically, one can update the data after the contract is originated using some special entry points. Nevertheless, if a data structure contains millions of existing values, it would be quite expensive and impractical.

Another idea is to have a special contract that would store the data and never need upgrading. **[Eternal storage][eth-eternal]** is a contract that provides a set of endpoints `get<T>(string variableName) -> Maybe T` and  `set<T>(string variableName, T value)`, where `<T>` is the value type we expect to set or get in return. In Ethereum implementation `<T>` is restricted to a set of primitive types, and the type safety is not guaranteed. Having a dedicated storage contract in Michelson poses additional challenges: it is not easy to view external contract storage in Michelson, and it would require the contract developers to drastically change the existing code.

**[Lazy upgrades][eth-lazy]** approach requires external calls as well but preserves type-safety. The basic idea is that the new version of a contract should call the previous reincarnation if it notices some data is missing. For instance, if one tries to transfer tokens from her balance, and there are no tokens allocated to the sender in the current version, the contract emits a view transaction to the previous version in order to find out the balance. While the approach seams appealing, the resulting code of the upgraded contract would have to branch on existing/non-existing data, emit operations, provide callbacks for view calls, etc. The complicated code can potentially lead to subtle vulnerabilities in the upgraded contract, and we want to avoid complexity as possible.

All the mechanisms described above assume that there is a contract administrator that rules the upgrades. In practice, administrator-forced upgrades are not applicable to some use-cases because they require a certain degree of trust in the person or organization that manages the contract. The users of the contract have to trust that the managing party handles the wallet keys safely and takes all the necessary precautions to prevent their leakage. Incidents like [Bancor hack][bancor-hack] have been reported showing practical evidence that upgrade administrator wallets can be compromised. In response to these threats, another contract upgrade paradigm – **user-defined upgrades** – has arisen.

The main idea of [user-defined upgrades][user-upgrade] is that the user of the contract is the only party that can choose whether to upgrade and transfer value to the next version of the contract or not. This paradigm offers additional benefit in case of project hard fork, since the user can decide which of the new versions to follow. On the other hand, if the original contract has a bug or vulnerability that threats users' funds or renders the contract unusable, user-defined upgrades can be less effective than administrator-forced ones.

## Upgradeable Lorentz contracts

In this document we present an administrator-forced upgrade mechanism for Lorentz contracts. It uses Ethereum's ideas that have already been applied to hundreds of contracts, Michelson's lambda functions, and an advanced type system offered by Lorentz to provide address immutability, implementation and storage upgrades, and _partial_ type safety.

The proposed mechanism offers type-safe storage access for each version of an upgradeable contract. Storage migrations and an interface are not type-safe, however: one operates on an untyped storage during migrations and may supply arguments of incorrect types during contract calls. These limitations can be overcome but the implementation of these extensions is out of scope of this document.

We define the storage and the interface of a proposed upgradeable contract as follows:
```
data Storage = Storage
  { dataMap :: UStore_
  , fields :: StorageFields
  }

data StorageFields = StorageFields
  { code  :: ContractCode
  , admin :: Address
  , currentVersion :: Natural
  , paused :: Bool
  }

type MigrationScript = Lambda UStore_ UStore_
type ContractCode = Lambda (ByteString, UStore_) ([Operation], UStore_)
type UStore_ = BigMap ByteString ByteString
```

```
data Parameter interface =
  = Upgrade UpgradeParameters
  | Run (UParam interface)
  | SetAdministrator Address
  | GetVersion (View () Natural)

type UpgradeParameters =
  ( "newVersion" :! Natural
  , "migrationScript" :! MigrationScript
  , "newCode" :! ContractCode
  )
```

Since one can not upgrade the contract interface (expressed as a sum type) after the contract has been deployed, the proposed upgradeable contract provides a `Run (UParam interface)` endpoint that runs the specified named endpoint. `UParam` is a type-safe wrapper over `(MText, ByteString)` pair, where the former is the name of an endpoint, and the latter is a packed argument. The dispatching algorithm of the upgradeable contract unpacks the arguments and passes the execution to the corresponding code block.

### Implementation upgrade

The proposed upgradeable contract interface has an `Upgrade` endpoint that accepts a new version of a contract. Prior to upgrade, the code of the new version must be split into endpoints with string identifiers. These endpoints are supplied as the `newCode` parameter of the upgrade.

The user also supplies a fallback function, a version identifier (equal to the current `version` + 1), and a migration script described in the next section. After a successful upgrade, the new version of the code is stored in `code`. The user of the contract may interact with the newly-deployed code using the `Run` endpoint we described earlier.

### Storage upgrade

While `UStore_` is just a big map, contract code uses a special parametrized type `UStore a` – a special type that represents a "generic storage". It resembles the idea of the Ethereum's Eternal storage but, unlike the latter, stores `pack`ed data and is modelled as a `BigMap ByteString ByteString` under the hood. Thus, `UStore_` can be coerced to `UStore a` when needed.

The `UStore` of a contract must have a parameter that we use to generate a type-safe interface. We call this parameter a `UStoreTemplate`. The contract code should not access `UStore_` directly but rather coerce `UStore_` to a concrete `UStore UStoreTemplate` and use `ustore{To,Get,Set}Field #label` instructions to access the fields.

`UStore_` is upgraded using migration scripts. `MigrationScript` is a lambda that performs all the necessary operations to transform the `UStore_` from an old `UStoreTemplate` to the new one, i.e. it:
  * deletes old unused fields,
  * adds new fields,
  * transfers data from old data structures to the new ones.

It is expected (though, not enforced) that after a migration is applied, the resulting `UStore` is compatible with the new version of `UStoreTemplate`.

### Versioning
When a contract gets upgraded, it is easy to make a mistake and supply a wrong version of the code and migration scripts. While one can easily roll back the changes in the code, migrations generally can not be rolled back by design.

To prevent inconsistent upgrades, an upgradeable contract stores `currentVersion :: Natural`, and provides a view endpoint `GetVersion (View () Natural)` so that the other contracts can check whether the contract they are calling provides the interface they expect. The `Upgrade` method accepts the new version number and checks whether the new version equals `currentVersion + 1`.

### Access control
To ensure security of the deployed contract, the access to the upgrade methods must be restricted. Ideally, any form of access control policy should be supported, including but not limited to:
  * upgrades that require multiple signatures;
  * collective voting on upgrade proposals;
  * time-limited upgradeability;
  * etc.

Luckily, we can achieve such flexibility easily by making a contract administered by another – access control – contract. The access control contract should mimic the interface of the contract it controls, and proxy the upgrade requests if the preconditions are met according to the specified access control policy. The controlled contract, in turn, must ensure that the transaction `SENDER` equals to the current contract `administrator` prior to upgrade. It should also provide a `SetAdministrator Address` endpoint in case the access control policy (e.g. a set of eligible approvers) needs to be revised.

A person administering an upgradeable contract can be treated as a special case of an access control policy contract – the one with unlimited rights for upgrades.

## Appendix A: Possible extensions
This section describes optional extensions that are not strictly required but may be beneficial to the contract authors and users. Some of these extensions are straightforward, while others may require further research or be non-trivial to implement.

#### Non-atomic upgrades
Tezos has an intrinsic `max_operation_size` limit which may be insufficient to originate a complex contract. One can overcome this limitation by slightly modifying the proposed upgradeable contract.

The solution is to fill in the code of the contract endpoint by endpoint rather than doing it in one transaction. The interface of this modified upgradeable contract should be the following:

```
data Parameter interface
  = Run (UParam interface)
  | Upgrade UpgradeParameters
  | GetVersion (View () Natural)
  | SetAdministrator Address

  -- Entrypoint-wise upgrades are currently not protected from version mismatch
  -- in subsequent transactions, so the user ought to be careful with them.
  -- This behavior may change in future if deemed desirable.
  | EpwBeginUpgrade Natural  -- version
  | EpwApplyMigration MigrationScript
  | EpwSetCode ContractCode
  | EpwFinishUpgrade
```

The new endpoints should have the following behavior:
  * `EpwBeginUpgrade` checks the supplied version and pauses all `Run` operations by setting a new `paused :: Bool` storage parameter to true.
  * `EpwApplyMigration` applies storage and code migrations; can be called multiple times if deemed necessary.
  * `EpwSetCode` sets the source code of the contract; it contains mostly dispatching logic since all the entrypoints are updated using `EpwApplyMigration`.
  * `EpwFinishUpgrade` unpauses the contract.

#### Type safe migrations
Instead of forcing our users to write migration scripts by hand, we can provide a safe interface for `UStoreTemplate` migrations. Such interface would:
  * allow the user to initialize the new fields based on the values in the old UStore,
  * explicitly specify the fields that should be unchanged,
  * automatically delete old unused fields.

(Note: The code below is provided to demonstrate the idea. It is not supposed to be used as a reference, the actual implementation may be completely different)

```
-- Version 1
data UStoreTemplate = UStoreTemplate
  { xy :: UStoreField (Integer, Integer)
  , z  :: UStoreField MText
  } deriving Generic

migration :: UStore :-> UStore
migration = makeMigration @() $
  UStoreTemplate
    { xy = UStoreField (1, 2)
    , z  = UStoreField [mt|Hello world|]
    }
```

```
-- Version 2
-- In v2 we split xy into x and y and leave z unchanged
data UStoreTemplate = UStoreTemplate
  { x :: UStoreMigratedField Integer
  , y :: UStoreMigratedField Integer
  , z :: UStoreMigratedField MText
  } deriving Generic

migration :: UStore :-> UStore
migration = makeMigration @V1.UStoreTemplate $
  UStoreTemplate
    { x = UStoreMigratedField migrateX
    , y = UStoreMigratedField migrateY
    , z = UStoreMigratedField $ V1.ustoreToField #z
    -- xy gets deleted automatically since it's not in V2.UStoreTemplate
    }

migrateX :: UStore ': s :-> Integer ': s
migrateX = V1.ustoreToField #xy # car

migrateY :: UStore ': s :-> Integer ': s
migrateY = V1.ustoreToField #xy # cdr
```

There are some drawbacks to this approach, though:
  1. Gas costs may be higher due to per-field migrations.
  1. The order of operations matters since the store is updated in-place. Thus, the user shouldn't be able to change some value or its type without changing the field name (e.g. make `z` `Natural` instead of `MText`), otherwise the fields that depend on `z` being `MText` may fail to upgrade.

#### Lazy BigMap upgrades
Currently, it is not possible to change the data type of a nested quasi-`BigMap`, e.g. if you have a `BigMap Address Investor` in UStore, you cannot add a new field to `Investor` during migration. This limitation can be dropped if a nested `BigMap` stored version information in values, i.e.
`BigMap Address (Version, Investor)`. If the `Version` of the value is not equal to the current version, we apply all per-field migrations as described earlier. This would require us to store migration lambdas for each version, though.

#### Advanced versioning
Instead of storing a `Natural` version, an upgradeable contract can store `currentVersion :: ContractVersion`, where the elements of the `ContractVersion { major :: Natural, minor :: Natural, bugfix :: Natural)` denote the elements of the semantic version identifier. The contract also exposes the corresponding `View` endpoint. During the upgrade the contract checks that the version identifier is increasing, one step at a time, i.e.:
  * Δmajor = 1 & minor = 0 & bugfix = 0, or
  * Δmajor = 0 & Δminor = 1 & bugfix = 0, or
  * Δmajor = 0 & Δminor = 0 & Δbugfix = 1

For example, 1.1.1→1.1.2 and 1.0.58→1.1.0 are valid migrations, whereas 1.1.1→1.2.1 and 1.0.58→1.1.58 are not.

The following restrictions on the public interface may be imposed:
  * Bugfix change — no changes in the public interface are allowed;
  * Minor change — the public interface may contain new methods, the old method names and parameters must remain the same;
  * Major change — the public interface may change as desired.

While these restrictions may help to avoid incompatibility issues, unexpected breaking changes still can occur in the implementation of the exposed methods.

<!-- References -->
[eth-dao]: https://medium.com/swlh/the-story-of-the-dao-its-history-and-consequences-71e6a8a551ee "The Story of the DAO — Its History and Consequences"
[neo-migrate]: https://docs.neo.org/en-us/sc/tutorial/migrate.html "Migrating Smart Contracts"
[eos-precautions]: https://eosio.stackexchange.com/questions/559/how-eos-prevent-contract-upgrade-for-evil "How eos prevent contract upgrade for evil? – EOS StackExchange"
[eth-yp]: https://ethereum.github.io/yellowpaper/paper.pdf "Ethereum Yellow Paper"
[eth-call-delegatecall]: https://ethereum.stackexchange.com/questions/3667/difference-between-call-callcode-and-delegatecall?noredirect=1&lq=1 "Difference between CALL, CALLCODE and DELEGATECALL – Ethereum StackExchange"
[eth-proxy]: https://fravoll.github.io/solidity-patterns/proxy_delegate.html "Proxy Delegate – Solidity patterns"
[eth-eternal]: https://fravoll.github.io/solidity-patterns/eternal_storage.html "Eternal Storage – Solidity patterns"
[eth-lazy]: https://medium.com/bitclave/the-easy-way-to-upgrade-smart-contracts-ba30ba012784 "The Easy Way to Upgrade Smart Contracts"
[user-upgrade]: https://medium.com/@k06a/the-safest-and-probably-the-best-way-to-upgrade-smart-contracts-ea6e619d5dfd "Upgradability is a BACKDOOR!!111!"
[bancor-hack]: https://twitter.com/Bancor/status/1016420621666963457 "Bancor on Twitter: Here is the latest update on the recent security breach:… "
