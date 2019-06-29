module Test.Lorentz.Contracts.UpgradeableCounter
  ( spec_UpgradeableCounter
  ) where

import Lorentz (View(..), MText, mt)

import Test.Hspec (Spec, describe, it)

import Lorentz.Constraints
import Lorentz.Contracts.Consumer (contractConsumer)
import Lorentz.Contracts.UpgradeableCounter
import qualified Lorentz.Contracts.UpgradeableCounter.V1 as V1
import qualified Lorentz.Contracts.UpgradeableCounter.V2 as V2
import Lorentz.Test
import Lorentz.Value
import Michelson.Interpret.Pack (packValue')
import qualified Michelson.Typed as T
import Util.Instances ()
import Util.Named ((.!))

admin, admin2, adversary :: Address
admin = genesisAddress1
admin2 = genesisAddress2
adversary = genesisAddress3

originateUpgradeableCounter
  :: IntegrationalScenarioM (ContractAddr Parameter)
originateUpgradeableCounter =
  lOriginate upgradeableCounterContract "UpgradeableCounter"
    (emptyMigration admin) (toMutez 1000)

originateUpgradeableCounterV1
  :: IntegrationalScenarioM (ContractAddr Parameter)
originateUpgradeableCounterV1 = do
  contract <- originateUpgradeableCounter
  withSender admin $ upgradeToV1 contract
  return contract

upgradeToV1 :: ContractAddr Parameter -> IntegrationalScenarioM ()
upgradeToV1 contract =
  lCall contract $ Upgrade
    ( #newVersion V1.version
    , #migrationScript V1.migrate
    , #newCode V1.counterContract
    )

upgradeToV2 :: ContractAddr Parameter -> IntegrationalScenarioM ()
upgradeToV2 contract =
  lCall contract $ Upgrade
    ( #newVersion V2.version
    , #migrationScript V2.migrate
    , #newCode V2.counterContract
    )

uCall
  :: forall a.
     (KnownValue a, IsoValue a, T.HasNoOp (ToT a), T.HasNoBigMap (ToT a))
  => ContractAddr Parameter -> MText -> a
  -> IntegrationalScenarioM ()
uCall contract method arg = do
  lCall contract $ Run (method, packValue' $ toVal arg)

getCounterValueV1
  :: ContractAddr Parameter
  -> ContractAddr Natural
  -> IntegrationalScenarioM ()
getCounterValueV1 contract consumer = do
  uCall contract [mt|GetCounterValue|] $ View () consumer

getVersion
  :: ContractAddr Parameter
  -> ContractAddr Natural
  -> IntegrationalScenarioM ()
getVersion contract consumer = do
  lCall contract $ GetVersion (View () consumer)

getCounterValueV2
  :: ContractAddr Parameter
  -> ContractAddr Integer
  -> IntegrationalScenarioM ()
getCounterValueV2 contract consumer = do
  uCall contract [mt|GetCounterValue|] $ View () consumer

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

        uCall contract [mt|Add|] (2 :: Natural)
        getCounterValueV1 contract consumer

        uCall contract [mt|Mul|] (3 :: Natural)
        getCounterValueV1 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [2, 6]

  describe "v2" $ do
    it "Upgrades to v2" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        getVersion contract consumer
        withSender admin $ upgradeToV2 contract
        getVersion contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [1, 2]

    it "Preserves the counter after the upgrade" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        uCall contract [mt|Add|] (42 :: Natural)
        withSender admin $ upgradeToV2 contract
        getCounterValueV2 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [42]

    it "Exposes new methods" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender admin $ upgradeToV2 contract
        uCall contract [mt|Inc|] ()
        uCall contract [mt|Inc|] ()
        getCounterValueV2 contract consumer
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [2, 1]

    it "Allows to decrement below zero" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        withSender admin $ upgradeToV2 contract
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer
        uCall contract [mt|Dec|] ()
        getCounterValueV2 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [-1, -2, -3]

  describe "Illegal migrations" $ do
    it "Cannot migrate to the same version" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        withSender admin $ upgradeToV1 contract
        validate . Left $
          lExpectError (== VersionMismatch (#expected .! 2, #actual .! 1))

    it "Cannot migrate to the wrong version" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounter
        withSender admin $ upgradeToV2 contract
        validate . Left $
          lExpectError (== VersionMismatch (#expected .! 1, #actual .! 2))

    it "Cannot migrate if sender is not admin" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounter
        withSender adversary $ upgradeToV1 contract
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
