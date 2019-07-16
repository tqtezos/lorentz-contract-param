module Lorentz.Contracts.UpgradeableCounter.V1
  ( Interface
  , version
  , migrate
  , migrations
  , counterContract
  , UStoreV1
  ) where

import Lorentz
import Prelude (foldl')

import Lorentz.Contracts.Upgradeable.EntryPointWise
import Lorentz.Contracts.Upgradeable.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

version :: Natural
version = 1

data UStoreTemplate = UStoreTemplate
  { counterValue :: UStoreField Natural
  , code :: MText |~> EntryPointImpl UStoreTemplate
  , fallback :: UStoreField $ EpwFallback UStoreTemplate
  } deriving stock (Eq, Generic)

type UStoreV1 = UStore UStoreTemplate

type Interface =
  [ "add" ?: Natural
  , "mul" ?: Natural
  , "getCounterValue" ?: View () Natural
  ]


runAdd :: Lambda (Natural, UStoreV1) ([Operation], UStoreV1)
runAdd = do
  unpair
  dip $ ustoreGetField #counterValue
  add
  ustoreSetField #counterValue
  nil; pair

runMul :: Lambda (Natural, UStoreV1) ([Operation], UStoreV1)
runMul = do
  unpair
  dip $ ustoreGetField #counterValue
  mul
  ustoreSetField #counterValue
  nil; pair

runGetCounterValue :: Lambda (View () Natural, UStoreV1) ([Operation], UStoreV1)
runGetCounterValue = do
  unpair
  view_ $ do
    cdr
    ustoreGetField #counterValue
    dip drop

epwContract :: EpwContract Interface UStoreTemplate
epwContract = mkEpwContractT
  ( #add /==> runAdd
  , #mul /==> runMul
  , #getCounterValue /==> runGetCounterValue
  ) epwFallbackFail

-- | Migrations represent entrypoint-wise upgrades. Each migration puts
--   an implementation of a method to UStore. The contract code itself
--   (`epwServe`) does not do anything special except for taking these
--   lambdas out of the big map.
migrations :: [MigrationScript]
migrations =
  migrateStorage :
  (epwCodeMigrations epwContract)

-- | This function migrates the storage from an empty one to UStoreV1,
--   i.e. it populates the empty BigMap with entries and initial values
--   for each field. Currently it is not guaranteed that all fields will be set
--   according to the template. See /docs/upgradeableContracts.md for type-safe
--   migrations idea description. The result is expected to adhere
--   to V1.UStoreTemplate.
migrateStorage :: MigrationScript
migrateStorage = do
  coerce_ @UStore_ @UStoreV1
  push @Natural 0
  ustoreSetField #counterValue
  coerce_ @UStoreV1 @UStore_

migrate :: MigrationScript
migrate = foldl' (#) nop migrations

counterContract :: ContractCode
counterContract = epwServe epwContract
