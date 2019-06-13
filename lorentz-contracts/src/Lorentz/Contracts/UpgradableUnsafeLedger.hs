-- | Hello world contract

module Lorentz.Contracts.UpgradableUnsafeLedger
  ( Parameter(..)
  , Storage(..)
  , upgradableUnsafeLedgerContract
  , emptyMigration
  ) where

import Lorentz (Contract)

import Lorentz.Contracts.Upgradable.Common

upgradableUnsafeLedgerContract :: Contract Parameter Storage
upgradableUnsafeLedgerContract = upgradableContract
