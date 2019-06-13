-- | Hello world contract

module Lorentz.Contracts.UpgradableCounter
  ( Parameter(..)
  , Storage(..)
  , upgradableCounterContract
  , emptyMigration
  ) where

import Lorentz (Contract)

import Lorentz.Contracts.Upgradable.Common

upgradableCounterContract :: Contract Parameter Storage
upgradableCounterContract = upgradableContract
