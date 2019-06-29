-- | Hello world contract

module Lorentz.Contracts.UpgradeableUnsafeLedger
  ( Parameter(..)
  , Storage
  , upgradeableUnsafeLedgerContract
  , emptyMigration
  ) where

import Lorentz (Contract)

import Lorentz.Contracts.Upgradeable.Common

upgradeableUnsafeLedgerContract :: Contract Parameter Storage
upgradeableUnsafeLedgerContract = upgradeableContract
