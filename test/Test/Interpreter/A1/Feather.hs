{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

-- | Tests for feathering as described in TZIP-A1.

module Test.Interpreter.A1.Feather
  ( featherSpec
  ) where

import Control.Lens (makeLenses, (%=), (+=), (-=), (.=))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Singletons (SingI)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, frequency, listOf, suchThat)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Instances.Natural ()

import Lorentz (View, Void_)
import qualified Lorentz as L
import Michelson.Interpret (MichelsonFailed(..))
import Michelson.Interpret.Pack (packValue')
import Michelson.Test
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Tezos.Address
import Tezos.Core
import Tezos.Crypto

data Outcome = Outcome
  { _oCounter :: !Natural
  , _oList :: ![Natural]
  , _oSum :: !Natural
  }

makeLenses ''Outcome

featherSpec :: Spec
featherSpec =
  specWithContractL "contracts/A1/counter.mtz" $ \counter ->
  specWithContractL "contracts/A1/feather.mtz" $ \feather ->
  specWithContractL "contracts/A1/caller-add.mtz" $ \callerAdd ->
  specWithContractL "contracts/A1/caller-append.mtz" $ \callerAppend ->
  specImpl counter feather callerAdd callerAppend

data CounterParameter
  = AntibumpCounter
  | BumpCounter
  | ResetCounter Natural
  | GetCount (View () Natural)
  | HashCount (Void_ () ByteString)
  deriving stock Generic
  deriving anyclass T.IsoValue
type CounterStorage = Natural

type FeatherParameter = ((), Maybe Natural)
type FeatherStorage = (Address, Maybe Address)

type CallerParameter = Either Natural ()
type CallerAddStorage = (Natural, Address)
type CallerAppendStorage = ([Natural], Address)

-- Actions that can be performed by this scenario.
data Action
  = Antibump
  -- ^ Antibump counter
  | Bump
  -- ^ Bump counter
  | Reset !Natural
  -- ^ Reset counter
  | HashCounter
  -- ^ Get counter's hash using void entry point (which implies that
  -- everything else should fail)
  | Add !Natural
  -- ^ Add a constant to caller-add
  | AddRecord
  -- ^ Add counter value to caller-add
  | Append !Natural
  -- ^ Append a constant to caller-append
  | AppendRecord
  -- ^ Append counter value to caller-append
  deriving (Show, Generic)

genAction :: Gen Action
genAction = genericArbitrary

data Fixture = Fixture
  { fActions :: ![Action]
  , fInitialCounter :: !Natural
  , fInitialList :: ![Natural]
  , fInitialSum :: !Natural
  } deriving (Show)

-- We generate 'HashCounter' with smaller probablity, because otherwise
-- tests will often stop quickly (they stop as soon as 'HashCounter'
-- occurs).
genFixture :: Gen Fixture
genFixture = do
  fActions <-
    listOf $ frequency
      [ (26, genAction `suchThat` notHashCounter)
      , (1, pure HashCounter)
      ]
  fInitialCounter <- arbitrary
  fInitialList <- arbitrary
  fInitialSum <- arbitrary
  pure Fixture {..}
  where
    notHashCounter =
      \case
        HashCounter -> False
        _ -> True

data ExpectedFail
  = NegativeCounter
  | HashCounterCalled !ByteString

expectedOutcome :: Fixture -> Either ExpectedFail Outcome
expectedOutcome Fixture {..} =
  runExcept $ execStateT (mapM_ step fActions) start
  where
    start = Outcome
      { _oCounter = fInitialCounter
      , _oList = fInitialList
      , _oSum = fInitialSum
      }
    step :: Action -> StateT Outcome (Except ExpectedFail) ()
    step = \case
      Antibump -> use oCounter >>= \case
        0 -> throwError NegativeCounter
        _ -> oCounter -= 1
      Bump -> oCounter += 1
      Reset x -> oCounter .= x
      HashCounter ->
        throwError .
        HashCounterCalled .
        sha512 .
        packValue' .
        T.toVal =<<
        use oCounter
      Add x -> oSum += x
      AddRecord -> (oSum +=) =<< use oCounter
      Append x -> oList %= (x:)
      AppendRecord -> do
        c <- use oCounter
        oList %= (c:)

specImpl ::
     (U.Contract, L.Contract CounterParameter CounterStorage)
  -> (U.Contract, L.Contract FeatherParameter FeatherStorage)
  -> (U.Contract, L.Contract CallerParameter CallerAddStorage)
  -> (U.Contract, L.Contract CallerParameter CallerAppendStorage)
  -> Spec
specImpl (counter, _) (feather, _) (callerAdd, _) (callerAppend, _) =
  prop "A mix of random actions is handled as expected" $
    forAll genFixture $ \fixture ->
      integrationalTestProperty (scenario fixture)
  where
    scenario :: Fixture -> IntegrationalScenario
    scenario fixture@Fixture {..} = do
      let
        initBalance = unsafeMkMutez 100
        -- TODO: probably should be moved into a more general place
        -- TODO: deal with this - use 'originateLorentz'
        toUntypedValue ::
          forall t x . (T.ToT x ~ t, T.IsoValue x, SingI t, T.HasNoOp t) => x -> U.Value
        toUntypedValue = T.untypeValue . T.toVal

      counterAddress <-
        originate counter "counter" (toUntypedValue @(T.ToT CounterStorage) fInitialCounter) initBalance

      let
        defaultFeatherValue =
          toUntypedValue @(T.ToT FeatherStorage) (counterAddress, Nothing @Address)

      featherAddress <-
        originate feather "feather" defaultFeatherValue initBalance
      cAddAddress <-
        originate callerAdd "caller-add"
        (toUntypedValue @(T.ToT CallerAddStorage) (fInitialSum, featherAddress)) initBalance
      cAppendAddress <-
        originate callerAppend "caller-append"
        (toUntypedValue @(T.ToT CallerAppendStorage) (fInitialList, featherAddress)) initBalance

      let
        transfer' ::
          forall t . (SingI t, T.HasNoOp t)
          => Address -> T.Value t -> IntegrationalScenarioM ()
        transfer' addr param =
          let txData = TxData
                { tdSenderAddress = genesisAddress
                , tdParameter = T.untypeValue param
                , tdAmount = unsafeMkMutez 0
                }
           in transfer txData addr

        transferToCounter :: CounterParameter -> IntegrationalScenarioM ()
        transferToCounter = transfer' counterAddress . T.toVal

        transferToAdd :: CallerParameter -> IntegrationalScenarioM ()
        transferToAdd = transfer' cAddAddress . T.toVal

        transferToAppend :: CallerParameter -> IntegrationalScenarioM ()
        transferToAppend = transfer' cAppendAddress . T.toVal

        performAction :: Action -> IntegrationalScenarioM ()
        performAction = \case
          Antibump -> transferToCounter AntibumpCounter
          Bump -> transferToCounter BumpCounter
          Reset x -> transferToCounter $ ResetCounter x
          HashCounter -> transferToCounter $ HashCount (L.mkVoid ())
          Add x -> transferToAdd $ Left x
          AddRecord -> transferToAdd $ Right ()
          Append x -> transferToAppend $ Left x
          AppendRecord -> transferToAppend $ Right ()

      mapM_ performAction fActions

      let
        checkHash :: ByteString -> MichelsonFailed -> Bool
        checkHash expectedHash = (== MichelsonFailedWith (T.toVal expectedHash))
        validator :: IntegrationalValidator
        validator = case expectedOutcome fixture of
          Left NegativeCounter ->
            Left $ expectMichelsonFailed (const True) counterAddress
          Left (HashCounterCalled expectedHash) ->
            Left $ expectMichelsonFailed (checkHash expectedHash) counterAddress
          Right (Outcome {..}) -> Right $ composeValidatorsList
            [ -- Feather must always be called twice and the second
              -- call must set its storage to its initial value.
              expectStorageConst featherAddress defaultFeatherValue
            , -- Counter, caller-add and caller-append have values
              -- according to 'Outcome'.
              expectStorageConst counterAddress
              (toUntypedValue @(T.ToT CounterStorage) _oCounter)
            , expectStorageConst cAddAddress
              (toUntypedValue @(T.ToT CallerAddStorage) (_oSum, featherAddress))
            , expectStorageConst cAppendAddress
              (toUntypedValue @(T.ToT CallerAppendStorage) (_oList, featherAddress))
            , -- Balances must not change
              expectBalance counterAddress initBalance
            , expectBalance featherAddress initBalance
            , expectBalance cAddAddress initBalance
            , expectBalance cAppendAddress initBalance
            ]

      validate validator
