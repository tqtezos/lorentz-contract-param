
module Util where

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

instance Arbitrary a => Arbitrary (NamedF Identity a name) where
  arbitrary = ArgF . Identity <$> arbitrary

instance Arbitrary a => Arbitrary (View a r) where
  arbitrary = View <$> arbitrary <*> arbitrary
  shrink (View param callbackAddr) = (`View` callbackAddr) <$> shrink param

deriving instance Arbitrary (T.ContractAddr p)

instance Arbitrary Babylon.Parameter where
  arbitrary = oneof
    [ Babylon.Transfer <$> arbitrary
    , Babylon.Approve <$> arbitrary
    , Babylon.GetAllowance <$> arbitrary
    , Babylon.GetBalance <$> arbitrary
    , Babylon.GetTotalSupply <$> arbitrary
    , Babylon.SetPause <$> arbitrary
    , Babylon.SetAdministrator <$> arbitrary
    , Babylon.GetAdministrator <$> arbitrary
    , Babylon.Mint <$> arbitrary
    , Babylon.Burn <$> arbitrary
    ]

instance Arbitrary UnsafeLedger.Parameter where
  arbitrary = oneof
    [ UnsafeLedger.Transfer <$> arbitrary
    , UnsafeLedger.GetTotalSupply <$> arbitrary
    , UnsafeLedger.GetBalance <$> arbitrary
    ]

instance Arbitrary Walker.Parameter where
  arbitrary = oneof
    [  return Walker.GoLeft
    ,  return Walker.GoRight
    ,  return Walker.GoUp
    ,  return Walker.GoDown
    ,  Walker.Boost <$> arbitrary
    ,  Walker.Reset <$> arbitrary
    ]

instance Arbitrary Proxy.Parameter0 where
  arbitrary = oneof
    [  Proxy.Approve <$> arbitrary
    ,  Proxy.Parameter1 <$> arbitrary
    ]

instance Arbitrary Proxy.Parameter1 where
  arbitrary = oneof
    [  Proxy.GetAllowance <$> arbitrary
    ,  Proxy.Parameter2 <$> arbitrary
    ]

instance Arbitrary Proxy.Parameter2 where
  arbitrary = oneof
    [  Proxy.GetBalance <$> arbitrary
    ,  Proxy.GetTotalSupply <$> arbitrary
    ]

instance Arbitrary Proxy.Parameter where
  arbitrary = oneof
    [  Proxy.Transfer <$> arbitrary
    ,  Proxy.Parameter0 <$> arbitrary
    ]

instance Arbitrary Athens.Parameter where
  arbitrary = oneof
    [ Athens.Transfer <$> arbitrary
    , Athens.TransferViaProxy <$> arbitrary
    , Athens.Approve <$> arbitrary
    , Athens.ApproveViaProxy <$> arbitrary
    , Athens.GetAllowance <$> arbitrary
    , Athens.GetBalance <$> arbitrary
    , Athens.GetTotalSupply <$> arbitrary
    , Athens.SetPause <$> arbitrary
    , Athens.SetAdministrator <$> arbitrary
    , Athens.GetAdministrator <$> arbitrary
    , Athens.Mint <$> arbitrary
    , Athens.Burn <$> arbitrary
    , Athens.SetProxy <$> arbitrary
    ]

deriving instance Eq Babylon.Parameter
deriving instance Eq UnsafeLedger.Parameter
deriving instance Eq Walker.Parameter
deriving instance Eq Proxy.Parameter
deriving instance Eq Proxy.Parameter0
deriving instance Eq Proxy.Parameter1
deriving instance Eq Proxy.Parameter2
deriving instance Eq Athens.Parameter

deriving instance Show Babylon.Parameter
deriving instance Show UnsafeLedger.Parameter
deriving instance Show Walker.Parameter
deriving instance Show Proxy.Parameter
deriving instance Show Proxy.Parameter0
deriving instance Show Proxy.Parameter1
deriving instance Show Proxy.Parameter2
deriving instance Show Athens.Parameter

