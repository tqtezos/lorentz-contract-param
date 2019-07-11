module Lorentz.Contracts.UpgradeableCounter.V2
  ( Interface
  , UStoreV2
  , version
  , migrate
  , migrations
  , counterContract
  ) where

import Lorentz
import Prelude (foldl')

import Lorentz.Contracts.Upgradeable.EntryPointWise
import Lorentz.Contracts.Upgradeable.Common
import Lorentz.Contracts.UpgradeableCounter.V1 (UStoreV1)

version :: Natural
version = 2

data UStoreTemplate = UStoreTemplate
  { newCounterValue :: UStoreField Integer
  , code :: MText |~> EntryPointImpl UStoreTemplate
  , fallback :: UStoreField $ EpwFallback UStoreTemplate
  } deriving stock (Eq, Generic)

type UStoreV2 = UStore UStoreTemplate

type Interface =
  [ "inc" ?: ()
  , "dec" ?: ()
  , "getCounterValue" ?: View () Integer
  ]

epwContract :: EpwContract Interface UStoreTemplate
epwContract = mkEpwContract
  (  #inc /==> runInc
  :& #dec /==> runDec
  :& #getCounterValue /==> runView
  :& RNil
  )  epwFallbackFail

addInt :: Integer -> Lambda ((), UStoreV2) ([Operation], UStoreV2)
addInt x = do
  unpair
  drop
  ustoreGetField #newCounterValue
  push x
  add
  ustoreSetField #newCounterValue
  nil; pair

runInc :: Lambda ((), UStoreV2) ([Operation], UStoreV2)
runInc = addInt 1

runDec :: Lambda ((), UStoreV2) ([Operation], UStoreV2)
runDec = addInt (-1)

runView :: Lambda (View () Integer, UStoreV2) ([Operation], UStoreV2)
runView = do
  unpair
  view_ $ do
    cdr
    ustoreGetField #newCounterValue
    dip drop


migrations :: [MigrationScript]
migrations =
  migrateStorage :
  removeOldEndpoints :
  (epwCodeMigrations epwContract)

removeOldEndpoints :: MigrationScript
removeOldEndpoints = do
  coerce_ @UStore_ @UStoreV1
  removeEndpoint #add
  removeEndpoint #mul
  coerce_ @UStoreV1 @UStore_

-- | This function migrates the storage from UStoreV1 to UStoreV2 through an
--   untyped representation (UStore_). Currently it is not type-safe. See
--   /docs/upgradeableContracts.md for type-safe migrations idea description.
--   The result is expected to adhere to V2.UStoreTemplate.
migrateStorage :: MigrationScript
migrateStorage = do
  coerce_ @UStore_ @UStoreV1
  ustoreGetField #counterValue
  int
  dip $ coerce_ @UStoreV1 @UStoreV2
  ustoreSetField #newCounterValue
  coerce_ @UStoreV2 @UStore_
  unsafeDeleteField [mt|counterValue|]
  where
    unsafeDeleteField fieldName = do
      ensureFieldExists fieldName
      none; push fieldName; pack; update
    ensureFieldExists fieldName = do
      dup; push fieldName; pack; get
      assertSome $ mconcat [[mt|Broken migrartion|], fieldName]; drop

migrate :: MigrationScript
migrate = foldl' (#) nop migrations

counterContract :: ContractCode
counterContract = epwServe epwContract
