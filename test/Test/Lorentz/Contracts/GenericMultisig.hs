
-- | Module, containing spec to test NatStorageContract.tz contract.
module Test.Lorentz.Contracts.GenericMultisig
  ( prop_arbitrarySizedSublist
  , test_WrappedMultisigContract
  ) where

import Test.QuickCheck (Arbitrary(..), Gen, Property, choose, counterexample, suchThat, (.&&.), (===), (.||.), (=/=), generate)
import Test.QuickCheck.Property (expectFailure, forAll, withMaxSuccess)
import Test.QuickCheck.Instances.Natural ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, ioProperty)

import GHC.Natural

import Lorentz (compileLorentz)
import Lorentz.Contracts.GenericMultisig
import Lorentz.Contracts.GenericMultisig.Wrapper (wrappedMultisigContractNat, initStorageWrappedMultisigContractNat)
import Michelson.Interpret (ContractEnv(..))
import Michelson.Test
  (ContractPropValidator, concatTestTrees, contractProp, midTimestamp, testTreesWithTypedContract)
import Michelson.Test.Dummy
import Michelson.Test.Util (failedProp)
import Michelson.Typed (ToT)
import qualified Michelson.Typed as T
import Tezos.Core (Mutez, unMutez, unsafeMkMutez, unsafeSubMutez)
import Tezos.Crypto (PublicKey, SecretKey, Signature, toPublic, sign)
import Michelson.Interpret.Pack (packValue')

type MultisigStorage = (T.BigMap Bool (), ((BigMapContract Bool () Natural Natural, Natural), Storage))

storageThreshold :: MultisigStorage -> Natural
storageThreshold = fst . snd . snd . snd

storageCounter :: MultisigStorage -> Natural
storageCounter = fst . snd . snd

storageValue :: MultisigStorage -> Natural
storageValue = snd . fst . snd

storagePublicKeys :: MultisigStorage -> [PublicKey]
storagePublicKeys = snd . snd . snd . snd

arbitraryWithSignersStorage :: Gen ([SecretKey], MultisigStorage)
arbitraryWithSignersStorage = do
  secretKeys <- arbitrary
  threshold <- naturalFromInteger <$> choose (0, genericLength secretKeys)
  multisigStorage <-
    initStorageWrappedMultisigContractNat <$> arbitrary <*> pure threshold <*>
    pure (toPublic <$> secretKeys)
  return (secretKeys, multisigStorage)

-- | Non-empty signers list and non-zero threshold
arbitraryWithNonEmptySignersStorage :: Gen ([SecretKey], MultisigStorage)
arbitraryWithNonEmptySignersStorage =
  suchThat arbitraryWithSignersStorage $ \ ~(secretKeys, storage) ->
    (not . null) secretKeys && ((/= 0) . storageThreshold) storage

arbitraryDefaultParam :: ContractEnv -> [SecretKey] -> MultisigStorage -> Gen (Mutez, Parameter Natural)
arbitraryDefaultParam _ _ _ = do
  (, Default) <$> arbitrary

-- | Generate a sublist with @gteThreshold@ `Just` elements,
-- where the list has `numKeys`
arbitrarySizedSublist :: Show a => Natural -> Natural -> [a] -> Gen [Maybe a]
arbitrarySizedSublist _ _ [] = return []
arbitrarySizedSublist _ 0 xs = error $ "arbitrarySizedSublist _ 0 " <> show xs
-- arbitrarySizedSublist 0 _ [] = return []
arbitrarySizedSublist 0 length' (_:_) = return $ genericReplicate length' Nothing
arbitrarySizedSublist lteLength length' xs
  | lteLength > length' = error $ "gteThreshold > numKeys: " <> show (lteLength, length')
  | lteLength == length' = return $ Just <$> xs
  | lteLength < length' =
    case xs of
      [] -> error $ "length' /= 0 but xs == []: " <> show length'
      (y:ys) -> do
        keepY <- arbitrary
        if keepY
           then (Just y :) <$> arbitrarySizedSublist (lteLength - 1) (length' - 1) ys
           else (Nothing :) <$> arbitrarySizedSublist lteLength (length' - 1) ys

-- | Ensure that the two lists have equal langth and:
--
-- @
--  map and . zipWith (maybe (const True) (==))
-- @
--
-- I.e. all values equal or left side `Nothing`
isSublistOf :: (Eq a, Show a)  => [Maybe a] -> [a] -> Property
isSublistOf xs ys = counterexample (show (xs, ys)) $
  case (xs, ys) of
    ([], []) -> () === ()
    (xs'@(_:_), []) -> xs' === []
    ([], ys'@(_:_)) -> [] === ys'
    (Nothing:xs', _:ys') -> isSublistOf xs' ys'
    (Just x:xs', y:ys') -> (x === y) .&&. isSublistOf xs' ys'

-- prop_arbitrarySizedSublist :: (Eq a, Show a) => Natural -> [a] -> Property
prop_arbitrarySizedSublist :: Natural -> [Bool] -> Property
prop_arbitrarySizedSublist i xs = ioProperty $ do
  ys <- generate $ arbitrarySizedSublist (i `mod` length') length' xs
  return . counterexample (show (i, xs, ys)) $ ys `isSublistOf` xs
  where
    length' = genericLength xs

-- | arbitraily sign up to or over threshold
arbitraryOperationSigned ::
     ContractEnv
  -> [SecretKey]
  -> MultisigStorage
  -> Gen (Mutez, Parameter Natural)
arbitraryOperationSigned ContractEnv {..} secretKeys storage = do
  let counter = fst . snd $ snd storage
  let threshold = fst . snd . snd $ snd storage
  let numKeys = genericLength secretKeys
  gteThreshold <-
    naturalFromInteger <$>
    choose (naturalToInteger threshold, naturalToInteger numKeys)
  gteSecretKeys <- arbitrarySizedSublist gteThreshold numKeys secretKeys
  (genericLength gteSecretKeys == gteThreshold) `unless`
    error
      (mconcat
         [ "genericLength gteSecretKeys < gteThreshold: "
         , show (length gteSecretKeys)
         , show gteThreshold
         ])
  newNat <- arbitrary
  let signNewNat =
        \secretKey ->
          sign secretKey . packValue' $
          T.toVal (ceSelf, (counter, Operation newNat))
  let signatures = fmap signNewNat <$> gteSecretKeys
  return (toEnum 0, MainParameter ((counter, Operation newNat), signatures))

updateM :: Monad m => (a -> m a) -> Int -> [a] -> m [a]
updateM _ i _ | i < 0 = error "i < 0"
updateM _ _ [] = return []
updateM f 0 (x:xs) = (: xs) <$> f x
updateM f i (x:xs) = (x :) <$> updateM f (i - 1) xs

arbitraryUpdate :: [a] -> (a -> Gen a) -> Gen [a]
arbitraryUpdate [] _ = return []
arbitraryUpdate xs f = do
  ix <- choose (0, length xs - 1)
  updateM f ix xs

-- | arbitrarily sign up to or over threshold, but invalidate one of the signatures
arbitraryOperationMisSigned :: ContractEnv -> [SecretKey] -> MultisigStorage -> Gen (Mutez, Parameter Natural)
arbitraryOperationMisSigned env secretKeys storage = do
  if null secretKeys
     then error "null secretKeys"
     else if storageThreshold storage == 0
     then error "storageThreshold storage == 0"
     else do
       (mutez', param') <- arbitraryOperationSigned env secretKeys storage
       case param' of
         Default -> error "Default parameter"
         MainParameter param -> do
           secretKey <- arbitrary `suchThat` (not . (`elem` secretKeys))
           signaturesWithMisSign <- arbitraryUpdate (snd param) $ \_ -> do
             let counter = storageCounter storage
             newNat <- (arbitrary :: Gen Natural)
             return . Just . sign secretKey . packValue' $ T.toVal (ceSelf env, (counter, Operation newNat))
           return (mutez', MainParameter $ signaturesWithMisSign <$ param)
      -- ensure at least one signer (or error, ensure from storage)
      -- ensure at least one included signature
      -- replace it with arbitrary signature (ensure not equal)

-- | arbitrarily sign under threshold
arbitraryOperationUnderSigned :: ContractEnv -> [SecretKey] -> MultisigStorage -> Gen (Mutez, Parameter Natural)
arbitraryOperationUnderSigned env secretKeys storage = do
  if null secretKeys
     then error "null secretKeys"
     else if storageThreshold storage == 0
     then error "storageThreshold storage == 0"
     else do
       (mutez', param') <- arbitraryOperationSigned env secretKeys storage
       case param' of
         Default -> error "Default parameter"
         MainParameter param -> do
           signaturesWithMisSign <- arbitraryUpdate (snd param) $ \_ ->
             return Nothing
           return (mutez', MainParameter $ signaturesWithMisSign <$ param)
  -- ensure at least one included signature
  -- remove at least one included signature

-- default entry point accepts 0 tez
-- default entry point accepts >0 tez
-- default entry point does not affect storage
--
-- non-default entry point rejects >0 tez
-- non-default entry point:
-- - if counters don't match, reject
-- - if counters match and action completes, counter storage incremented
-- - if list of public keys doesn't match, reject
-- - if fewer signatures than threshold, reject
-- - if singnature doesn't match, reject
-- - if sufficient number of signatures and all match, contract gets updated
test_WrappedMultisigContract :: IO [TestTree]
test_WrappedMultisigContract = concatTestTrees
  [ one . testGroup "arbitrarySizedSublist" <$>
    pure [testProperty "arbitrarySizedSublist isSublistOf input list" $ withMaxSuccess 200 $
            prop_arbitrarySizedSublist
         ]
  , one . testGroup "Michelson version" <$>
    testTreesWithTypedContract "contracts/WrappedMultisigContractNat.tz" wrappedMultisigContractTest
  , one . testGroup "Lorentz version" <$>
    wrappedMultisigContractTest (compileLorentz wrappedMultisigContractNat)
  , one . testGroup "Michelson version" <$>
    testTreesWithTypedContract "contracts/WrappedMultisigContractNat.tz" wrappedMultisigContractTestMalSigned
  , one . testGroup "Lorentz version" <$>
    wrappedMultisigContractTestMalSigned (compileLorentz wrappedMultisigContractNat)
  ]
  where
    wrappedMultisigContractTest contract =
      pure
      [ testProperty "Default param accepts Mutez" $ withMaxSuccess 200 $
          qcProp contract arbitraryDefaultParam arbitraryWithSignersStorage
      , testProperty "Signed Operation accepted" $ withMaxSuccess 200 $
          qcProp contract arbitraryOperationSigned arbitraryWithSignersStorage
      ]

    wrappedMultisigContractTestMalSigned contract =
      pure
      [ testProperty "Mis-Signed Operation rejected" $ withMaxSuccess 200 $ expectFailure $
          qcProp contract arbitraryOperationMisSigned arbitraryWithNonEmptySignersStorage
      , testProperty "Under-Signed Operation rejected" $ withMaxSuccess 200 $ expectFailure $
          qcProp contract arbitraryOperationUnderSigned arbitraryWithNonEmptySignersStorage
      ]

    qcProp ::
         T.Contract (ToT (Parameter Natural)) (ToT MultisigStorage)
      -> (ContractEnv -> [SecretKey] -> MultisigStorage -> Gen (Mutez, Parameter Natural))
      -> Gen ([SecretKey], MultisigStorage)
      -> Property
    qcProp contract paramGen storGen =
      forAll paramStoreGen $ \(mutez', parameter, keys', storage) ->
        let env' = env {ceAmount = mutez'}
         in let validate =
                  liftM2 (.&&.) (validateGenericMultisigStorage parameter storage) $
                  validateGenericMultisig env' parameter keys' storage
             in contractProp contract validate env' parameter storage
      where
        paramStoreGen = do
          (keys', storage) <- storGen
          (mutez', parameter) <- paramGen env keys' storage
          return (mutez', parameter, keys', storage)

    env = dummyContractEnv
            { ceNow = midTimestamp
            , ceAmount = unsafeMkMutez midAmount
            }
    midAmount = unMutez (maxBound `unsafeSubMutez` minBound) `div` 2

-- | Assert that new storage value is given parameter
validateGenericMultisig ::
     ContractEnv
  -> Parameter Natural
  -> [SecretKey]
  -> MultisigStorage
  -> ContractPropValidator (ToT MultisigStorage) Property
validateGenericMultisig _ =
  \case
    Default -> validateGenericMultisigDefault
    MainParameter ((counter, action'), signatures) ->
      case action' of
        Operation newNat ->
          validateGenericMultisigOperation counter signatures newNat
        ChangeKeys (newThreshold, newKeys) ->
          validateGenericMultisigChangeKeys
            counter
            signatures
            newThreshold
            newKeys

-- | Validate the default entrypoint, ensuring that storage is unchanged
validateGenericMultisigDefault ::
     [SecretKey]
  -> MultisigStorage
  -> ContractPropValidator (ToT MultisigStorage) Property
validateGenericMultisigDefault _ oldStorage (Left err, _) =
  failedProp $
  "validateGenericMultisigDefault: Unexpected script failure: " <>
  show (oldStorage, err)
validateGenericMultisigDefault _ oldStorage (Right (_, result), _) =
  counterexample "storage updated from default entrypoint" $
  T.toVal oldStorage === result

validateGenericMultisigOperation ::
     Natural
  -> [Maybe Signature]
  -> Natural
  -> [SecretKey]
  -> MultisigStorage
  -> ContractPropValidator (ToT MultisigStorage) Property
validateGenericMultisigOperation _ _ _ _ oldStorage (Left err, _) =
  failedProp $
  "validateGenericMultisigOperation: Unexpected script failure: " <>
  show (oldStorage, err)
validateGenericMultisigOperation counter signatures newNat secretKeys oldStorage (Right (_, result), _) =
  (counterexample "storage counter not incremented" $
   storageCounter oldStorage =/= storageCounter (T.fromVal result)) .&&.
  (counterexample "parameter counter not incremented" $
   counter =/= storageCounter (T.fromVal result)) .&&.
  (counterexample "threshold modified during operation" $
   storageThreshold oldStorage === storageThreshold (T.fromVal result)) .&&.
  (counterexample "not as many signatures as secret keys" $
   length signatures === length secretKeys) .&&.
  (counterexample "nat storage not updated" $
   newNat === storageValue (T.fromVal result))

validateGenericMultisigChangeKeys ::
     Natural
  -> [Maybe Signature]
  -> Natural
  -> [PublicKey]
  -> [SecretKey]
  -> MultisigStorage
  -> ContractPropValidator (ToT MultisigStorage) Property
validateGenericMultisigChangeKeys = error "test case unimplemented"

-- | Ensure:
-- - the counter is unchanged or incremented by one
-- - the bigmap is not affected
-- - the contract lambda is not affected
validateGenericMultisigStorage ::
     Parameter Natural
  -> MultisigStorage
  -> ContractPropValidator (ToT MultisigStorage) Property
validateGenericMultisigStorage Default oldStorage (Left err, _) =
  failedProp $
  "validateGenericMultisigStorage: Unexpected script failure: " <> show (oldStorage, err)
validateGenericMultisigStorage (MainParameter _) oldStorage (Left err, _) =
  failedProp $
  "validateGenericMultisigStorage: Unexpected script failure: " <> show (oldStorage, err)
validateGenericMultisigStorage _ oldStorage (Right (_, result), _) =
  (counterexample "new counter /= old counter + 0 || old counter + 1" $
   (oldCounter === newCounter) .||. (oldCounter + 1 === newCounter)) .&&.
  (counterexample "big map updated" $ oldBigMap === newBigMap) .&&.
  (counterexample "contract lambda updated" $
   oldContractLambda === newContractLambda)
  where
    newStorage :: MultisigStorage
    newStorage = T.fromVal result

    oldCounter, newCounter :: Natural
    oldCounter = fst . snd $ snd oldStorage
    newCounter = fst . snd $ snd newStorage

    oldBigMap, newBigMap :: T.BigMap Bool ()
    oldBigMap = fst oldStorage
    newBigMap = fst newStorage

    oldContractLambda, newContractLambda :: BigMapContract Bool () Natural Natural
    oldContractLambda = fst . fst $ snd oldStorage
    newContractLambda = fst . fst $ snd newStorage

