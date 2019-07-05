-- | UpgradeableUnsafeLedger demonstrates a real-world use case of
--   an upgradeable contract. Its V1 contains a bug that makes it return
--   an incorrect totalSupply. Fortunately, we can upgrade the contract and fix
--   the bug. This contract does not use entrypoint-wise upgrades. Instead, it
--   upgrades atomically in one transaction. For non-atomic upgrades please
--   see the UpgradeableCounter example.

module Lorentz.Contracts.UpgradeableUnsafeLedger
  ( Parameter(..)
  , Storage
  , upgradeableUnsafeLedgerContract
  , mkEmptyStorage
  ) where

import Lorentz (Contract, EntryPointKind)

import Lorentz.Contracts.Upgradeable.Common

upgradeableUnsafeLedgerContract
  :: forall (interface :: [EntryPointKind]).
     Contract (Parameter interface) Storage
upgradeableUnsafeLedgerContract = upgradeableContract
