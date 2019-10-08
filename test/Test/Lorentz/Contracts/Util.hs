
module Test.Lorentz.Contracts.Util where

import Test.QuickCheck (Arbitrary(..), Gen, Property, choose, counterexample, suchThat, (.&&.), (===), (.||.), (=/=), generate, oneof, property)
import Test.QuickCheck.Property (expectFailure, forAll, withMaxSuccess)
import Test.QuickCheck.Instances.Natural ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, ioProperty)
import Test.QuickCheck.Instances.Text

import GHC.Natural
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Lorentz (View(..), compileLorentz)
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

import Data.ByteArray (ByteArray(..))
import Data.Aeson
import Named
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random.Types (MonadRandom(..))
import Data.Singletons
import Data.Singletons.TypeLits
import qualified Data.Text as T

import Tezos.Address
import Tezos.Crypto
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon
import qualified Lorentz.Contracts.ManagedLedger.Proxy as Proxy
import qualified Lorentz.Contracts.UnsafeLedger as UnsafeLedger
import qualified Lorentz.Contracts.Walker as Walker

instance MonadRandom Gen where
  -- getRandomBytes :: ByteArray byteArray => Int -> Gen byteArray
  getRandomBytes n = snd . unsafePerformIO <$> do
    bytes <- replicateM n (arbitrary :: Gen Word8)
    return . allocRet n $ \ptr ->
      zip [0..] bytes `forM_` uncurry (pokeByteOff ptr)

instance Arbitrary Ed25519.SecretKey where
  arbitrary = Ed25519.generateSecretKey

instance Arbitrary Ed25519.PublicKey where
  arbitrary = Ed25519.toPublic <$> arbitrary

testFromToJSON :: forall a. (ToJSON a, FromJSON a, Eq a, Show a, Arbitrary a) => Property
testFromToJSON = property $ \(xs :: a) -> either (failedProp . T.pack) id $ do
  ys <- eitherDecode (encode xs)
  return $ xs === ys

prop_fromToJSONEd25519_PublicKey :: Property
prop_fromToJSONEd25519_PublicKey = testFromToJSON @Ed25519.PublicKey

testFromToJSONKey :: forall a. (ToJSONKey a, FromJSONKey a, Ord a, Show a, Arbitrary a) => Property
testFromToJSONKey = testFromToJSON @(Map a ())

prop_fromToJSONKeyEd25519_PublicKey :: Property
prop_fromToJSONKeyEd25519_PublicKey = testFromToJSONKey @Ed25519.PublicKey

prop_fromToJSONKeyPublicKey :: Property
prop_fromToJSONKeyPublicKey = testFromToJSONKey @PublicKey

test_fromToJSON :: IO TestTree
test_fromToJSON = testGroup "FromJSON(Key), ToJSON(Key)" <$> pure fromToJSONTests
  where
    fromToJSONTests =
      [ testProperty "() Ed25519_PublicKey" . withMaxSuccess 200 $ prop_fromToJSONEd25519_PublicKey
      , testProperty "(Key) Ed25519_PublicKey" . withMaxSuccess 200 $ prop_fromToJSONKeyEd25519_PublicKey
      , testProperty "(Key) PublicKey" . withMaxSuccess 200 $ prop_fromToJSONKeyPublicKey
      ]

