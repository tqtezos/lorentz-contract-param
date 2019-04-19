
-- | Tests for feathering as described in TZIP-A1.

module Test.Interpreter.A1.Feather
  ( featherSpec
  ) where

import Control.Lens (makeLenses, (%=), (+=), (-=), (.=))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Singletons (SingI)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, listOf)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Instances.Natural ()

import Michelson.Test
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Tezos.Address
import Tezos.Core

data Outcome = Outcome
  { _oCounter :: !Natural
  , _oList :: ![Natural]
  , _oSum :: !Natural
  }

makeLenses ''Outcome

featherSpec :: Spec
featherSpec =
  specWithContract "contracts/A1/counter.mtz" $ \counter ->
  specWithContract "contracts/A1/feather.mtz" $ \feather ->
  specWithContract "contracts/A1/caller-add.mtz" $ \callerAdd ->
  specWithContract "contracts/A1/caller-append.mtz" $ \callerAppend ->
  specImpl counter feather callerAdd callerAppend

-- TODO: add to Lorenz
type family MultiOr (ts :: [T.T]) :: T.T where
  MultiOr '[] = 'T.TUnit
  MultiOr (t:t1:'[]) = 'T.TOr t t1
  MultiOr (t:ts) = 'T.TOr t (MultiOr ts)

-- TODO: add to Lorenz
type family View (a :: T.T) (r :: T.T) :: T.T where
  View a r = 'T.TPair a ('T.TContract ('T.TPair a ('T.TOption r)))

type CounterParameter =
  MultiOr
  [ 'T.TUnit
  , 'T.TUnit
  , 'T.Tc 'T.CNat
  , View 'T.TUnit ('T.Tc 'T.CNat)
  ]
type CounterStorage = T.ToT Natural

type FeatherParameter = T.ToT ((), Maybe Natural)
type FeatherStorage = T.ToT (Address, Maybe Address)

type CallerParameter = T.ToT (Either Natural ())
type CallerAddStorage = T.ToT ((Natural, Address))
type CallerAppendStorage = T.ToT (([Natural], Address))

-- Actions that can be performed by this scenario.
data Action
  = Antibump
  -- ^ Antibump counter
  | Bump
  -- ^ Bump counter
  | Reset !Natural
  -- ^ Reset counter
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

genFixture :: Gen Fixture
genFixture = do
  fActions <- listOf genAction
  fInitialCounter <- arbitrary
  fInitialList <- arbitrary
  fInitialSum <- arbitrary
  pure Fixture {..}

data NegativeCounter = NegativeCounter

expectedOutcome :: Fixture -> Either NegativeCounter Outcome
expectedOutcome Fixture {..} =
  runExcept $ execStateT (mapM_ step fActions) start
  where
    start = Outcome
      { _oCounter = fInitialCounter
      , _oList = fInitialList
      , _oSum = fInitialSum
      }
    step :: Action -> StateT Outcome (Except NegativeCounter) ()
    step = \case
      Antibump -> use oCounter >>= \case
        0 -> throwError NegativeCounter
        _ -> oCounter -= 1
      Bump -> oCounter += 1
      Reset x -> oCounter .= x
      Add x -> oSum += x
      AddRecord -> (oSum +=) =<< use oCounter
      Append x -> oList %= (x:)
      AppendRecord -> do
        c <- use oCounter
        oList %= (c:)

specImpl ::
     (U.Contract, T.Contract CounterParameter CounterStorage)
  -> (U.Contract, T.Contract FeatherParameter FeatherStorage)
  -> (U.Contract, T.Contract CallerParameter CallerAddStorage)
  -> (U.Contract, T.Contract CallerParameter CallerAppendStorage)
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
        toUntypedValue ::
          forall t x . (T.ToT x ~ t, T.ToVal x, SingI t, T.HasNoOp t) => x -> U.Value
        toUntypedValue = T.untypeValue . T.toVal

      counterAddress <-
        originate counter (toUntypedValue @CounterStorage fInitialCounter) initBalance

      let
        defaultFeatherValue =
          toUntypedValue @FeatherStorage (counterAddress, Nothing @Address)

      featherAddress <-
        originate feather defaultFeatherValue initBalance
      cAddAddress <-
        originate callerAdd
        (toUntypedValue @CallerAddStorage (fInitialSum, featherAddress)) initBalance
      cAppendAddress <-
        originate callerAppend
        (toUntypedValue @CallerAppendStorage (fInitialList, featherAddress)) initBalance

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

        transferToCounter :: T.Value CounterParameter -> IntegrationalScenarioM ()
        transferToCounter = transfer' counterAddress

        transferToAdd :: T.Value CallerParameter -> IntegrationalScenarioM ()
        transferToAdd = transfer' cAddAddress

        transferToAppend :: T.Value CallerParameter -> IntegrationalScenarioM ()
        transferToAppend = transfer' cAppendAddress

        performAction :: Action -> IntegrationalScenarioM ()
        performAction = \case
          Antibump -> transferToCounter $
            T.VOr $ Left T.VUnit
          Bump -> transferToCounter $
            T.VOr $ Right $
            T.VOr $ Left T.VUnit
          Reset x -> transferToCounter $
            T.VOr $ Right $
            T.VOr $ Right $
            T.VOr $ Left (T.toVal x)
          Add x -> transferToAdd $
            T.VOr $ Left (T.toVal x)
          AddRecord -> transferToAdd $
            T.VOr $ Right T.VUnit
          Append x -> transferToAppend $
            T.VOr $ Left (T.toVal x)
          AppendRecord -> transferToAppend $
            T.VOr $ Right T.VUnit

      mapM_ performAction fActions

      let
        validator :: IntegrationalValidator
        validator = case expectedOutcome fixture of
          Left NegativeCounter -> Left $ expectMichelsonFailed counterAddress
          Right (Outcome {..}) -> Right $ composeValidatorsList
            [ -- Feather must always be called twice and the second
              -- call must set its storage to its initial value.
              expectStorageConst featherAddress defaultFeatherValue
            , -- Counter, caller-add and caller-append have values
              -- according to 'Outcome'.
              expectStorageConst counterAddress
              (toUntypedValue @CounterStorage _oCounter)
            , expectStorageConst cAddAddress
              (toUntypedValue @CallerAddStorage (_oSum, featherAddress))
            , expectStorageConst cAppendAddress
              (toUntypedValue @CallerAppendStorage (_oList, featherAddress))
            , -- Balances must not change
              expectBalance counterAddress initBalance
            , expectBalance featherAddress initBalance
            , expectBalance cAddAddress initBalance
            , expectBalance cAppendAddress initBalance
            ]

      validate validator
