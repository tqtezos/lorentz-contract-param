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


originateUpgradeableCounter
  :: IntegrationalScenarioM (ContractAddr Parameter)
originateUpgradeableCounter =
  lOriginate upgradeableCounterContract "UpgradeableCounter"
    emptyMigration (toMutez 1000)

originateUpgradeableCounterV1
  :: IntegrationalScenarioM (ContractAddr Parameter)
originateUpgradeableCounterV1 = do
  contract <- originateUpgradeableCounter
  lCall contract $ Upgrade
    ( #newVersion V1.version
    , #migrationScript V1.migrate
    , #newCode V1.counterContract
    )
  return contract

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
        lCall contract $ Upgrade
          ( #newVersion V2.version
          , #migrationScript V2.migrate
          , #newCode V2.counterContract
          )
        getVersion contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [1, 2]

    it "Preserves the counter after the upgrade" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        uCall contract [mt|Add|] (42 :: Natural)
        lCall contract $ Upgrade
          ( #newVersion V2.version
          , #migrationScript V2.migrate
          , #newCode V2.counterContract
          )
        getCounterValueV2 contract consumer

        validate . Right $
          lExpectViewConsumerStorage consumer [42]

    it "Exposes new methods" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounterV1
        consumer <- lOriginateEmpty contractConsumer "consumer"

        lCall contract $ Upgrade
          ( #newVersion V2.version
          , #migrationScript V2.migrate
          , #newCode V2.counterContract
          )
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

        lCall contract $ Upgrade
          ( #newVersion V2.version
          , #migrationScript V2.migrate
          , #newCode V2.counterContract
          )
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

        lCall contract $ Upgrade
          ( #newVersion V1.version
          , #migrationScript V1.migrate
          , #newCode V1.counterContract
          )

        validate . Left $
          lExpectError (== VersionMismatch (#expected .! 2, #actual .! 1))

    it "Cannot migrate to the wrong version" $ do
      integrationalTestProperty $ do
        contract <- originateUpgradeableCounter

        lCall contract $ Upgrade
          ( #newVersion V2.version
          , #migrationScript V2.migrate
          , #newCode V2.counterContract
          )

        validate . Left $
          lExpectError (== VersionMismatch (#expected .! 1, #actual .! 2))
