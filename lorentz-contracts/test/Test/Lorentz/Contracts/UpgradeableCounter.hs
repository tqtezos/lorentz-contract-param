module Test.Lorentz.Contracts.UpgradeableCounter
  ( spec_UpgradeableCounter
  ) where

import Lorentz (View(..))

import Test.Hspec (Spec, describe, it)
import Data.Vinyl.Derived (Label)
import Data.Coerce (coerce)
import GHC.TypeLits (KnownSymbol)

import Lorentz.Constraints
import Lorentz.Contracts.Consumer (contractConsumer)
import Lorentz.Contracts.UpgradeableCounter
import qualified Lorentz.Contracts.UpgradeableCounter.V1 as V1
import qualified Lorentz.Contracts.UpgradeableCounter.V2 as V2
import Lorentz.Test
import Lorentz.UParam
import Lorentz.Value
import Util.Instances ()
import Util.Named ((.!))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

admin, admin2, adversary :: Address
admin = genesisAddress1
admin2 = genesisAddress2
adversary = genesisAddress3

originateUpgradeableCounter
  :: IntegrationalScenarioM (ContractAddr (Parameter '[]))
originateUpgradeableCounter =
  lOriginate upgradeableCounterContract "UpgradeableCounter"
    (mkEmptyStorage admin) (toMutez 1000)

originateUpgradeableCounterV1
  :: IntegrationalScenarioM (ContractAddr (Parameter V1.Interface))
originateUpgradeableCounterV1 = do
  contract <- originateUpgradeableCounter
  withSender admin $ upgradeToV1 contract
  return (coerce contract)

-- We deliberately use forall here so that we can test incorrect upgrades
upgradeToV1
  :: forall (interface :: [EntryPointKind]).
     ContractAddr (Parameter interface)
  -> IntegrationalScenarioM (ContractAddr (Parameter V1.Interface))
upgradeToV1 contract = do
  lCall contract $ EpwBeginUpgrade V1.version
  forM_ V1.migrations (\m -> lCall contract $ EpwApplyMigration m)
  lCall contract $ EpwSetCode V1.counterContract
  lCall contract $ EpwFinishUpgrade
  return (coerce contract)

-- We deliberately use forall here so that we can test incorrect upgrades
upgradeToV2
  :: forall (interface :: [EntryPointKind]).
     ContractAddr (Parameter interface)
  -> IntegrationalScenarioM (ContractAddr (Parameter V2.Interface))
upgradeToV2 contract = do
  lCall contract $ EpwBeginUpgrade V2.version
  forM_ V2.migrations (\m -> lCall contract $ EpwApplyMigration m)
  lCall contract $ EpwSetCode V2.counterContract
  lCall contract $ EpwFinishUpgrade
  return (coerce contract)

uCall
  :: forall a name (interface :: [EntryPointKind]).
  ( IsoValue a, KnownValue a, NoOperation a, NoBigMap a
  , KnownSymbol name
  , RequireUniqueEntryPoints interface
  , LookupEntryPoint name interface ~ a
  )
  => ContractAddr (Parameter interface)
  -> Label name
  -> a
  -> IntegrationalScenarioM ()
uCall contract method arg = do
  lCall contract $ Run ((mkUParam method arg) :: UParam interface)

getCounterValueV1
  :: ContractAddr (Parameter V1.Interface)
  -> ContractAddr Natural
  -> IntegrationalScenarioM ()
getCounterValueV1 contract consumer = do
  uCall contract #getCounterValue $ View () consumer

getVersion
  :: forall (interface :: [EntryPointKind]).
     ContractAddr (Parameter interface)
  -> ContractAddr Natural
  -> IntegrationalScenarioM ()
getVersion contract consumer = do
  lCall contract $ GetVersion (View () consumer)

getCounterValueV2
  :: ContractAddr (Parameter V2.Interface)
  -> ContractAddr Integer
  -> IntegrationalScenarioM ()
getCounterValueV2 contract consumer = do
  uCall contract #getCounterValue $ View () consumer

spec_UpgradeableCounter :: Spec
spec_UpgradeableCounter = do
  describe "v1" $ do
    it "Initially contains zero" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"
        getCounterValueV1 contract consumer
        validate . Right $
          lExpectViewConsumerStorage consumer [0]

    it "Updates counter after each operation" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        uCall contract #add (2 :: Natural)
        getCounterValueV1 contract consumer

        uCall contract #mul (3 :: Natural)
        getCounterValueV1 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [2, 6]

  describe "v2" $ do
    it "Upgrades to v2" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        getVersion contract consumer
        contract2 <- withSender admin $ upgradeToV2 contract
        getVersion contract2 consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [1, 2]

    it "Preserves the counter after the upgrade" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        uCall contract #add (42 :: Natural)
        contract2 <- withSender admin $ upgradeToV2 contract
        getCounterValueV2 contract2 consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [42]

    it "Exposes new methods" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        contract2 <- withSender admin $ upgradeToV2 contract
        uCall contract2 #inc ()
        uCall contract2 #inc ()
        getCounterValueV2 contract2 consumer
        uCall contract2 #dec ()
        getCounterValueV2 contract2 consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [2, 1]

    it "Allows to decrement below zero" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        contract2 <- withSender admin $ upgradeToV2 contract
        uCall contract2 #dec ()
        getCounterValueV2 contract2 consumer
        uCall contract2 #dec ()
        getCounterValueV2 contract2 consumer
        uCall contract2 #dec ()
        getCounterValueV2 contract2 consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [-1, -2, -3]

  describe "Illegal migrations" $ do
    it "Cannot migrate to the same version" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        _ <- withSender admin $ upgradeToV1 contract
        validate . Left $
          lExpectError (== VersionMismatch (#expected .! 2, #actual .! 1))

    it "Cannot migrate to the wrong version" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounter
        _ <- withSender admin $ upgradeToV2 contract
        validate . Left $
          lExpectError (== VersionMismatch (#expected .! 1, #actual .! 2))

    it "Cannot migrate if sender is not admin" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounter
        _ <- withSender adversary $ upgradeToV1 contract
        validate . Left $
          lExpectError (== SenderIsNotAdmin)

  describe "Administrator change" $ do
    it "Admin can set a new administrator" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounter
        withSender admin . lCall contract $ SetAdministrator admin2
        withSender admin2 $ upgradeToV1 contract
        validate $ Right expectAnySuccess

    it "Non-admin cannot set a new administrator" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounter
        withSender adversary . lCall contract $ SetAdministrator admin2
        validate . Left $
          lExpectError (== SenderIsNotAdmin)
