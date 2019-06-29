module Lorentz.Contracts.Upgradeable.Common.Contract
  ( Parameter(..)
  , Storage(..)
  , Error(..)
  , upgradeableContract
  , emptyMigration
  ) where

import Lorentz

import Fmt (Buildable (..), (+|), (|+))
import qualified Data.Map as M

import qualified Michelson.Typed as T
import Util.Instances ()

import Lorentz.Contracts.Upgradeable.Common.Base

data Parameter
  = Run UParameter
  | Upgrade UpgradeParameters
  | GetVersion (View () Natural)
  deriving stock Generic
  deriving anyclass IsoValue

type UpgradeParameters =
  ( "newVersion" :! Natural
  , "migrationScript" :! MigrationScript
  , "newCode" :! ContractCode
  )

data Storage = Storage
  { dataMap :: UStore_
  , code :: ContractCode
  , currentVersion :: Natural
  }
  deriving stock Generic
  deriving anyclass IsoValue

data Error
  = SenderIsNotAdmin
    -- ^ Tx sender does not have enough rights to perform this operation.
  | VersionMismatch ("expected" :! Natural, "actual" :! Natural)
    -- ^ Expected version does not match the version of the supplied code.
  deriving stock (Eq, Generic)

deriveCustomError ''Error

instance Buildable Error where
  build = \case
    SenderIsNotAdmin ->
      "This operation can be executed only by admin, but is invoked by \
      \someone else"
    VersionMismatch (arg #expected -> expected, arg #actual -> actual) ->
      "The expected version (v" +| expected |+
      ") does not match the version of the supplied code (v" +| actual |+ ")"

userFail :: forall name fieldTy s s'. FailUsingArg Error name fieldTy s s'
userFail = failUsingArg @Error @name

emptyCode :: ContractCode
emptyCode = unpair # drop # nil # pair

emptyMigration :: Storage
emptyMigration = Storage
  { dataMap = T.BigMap $ M.fromList []
  , code = emptyCode
  , currentVersion = 0
  }

upgradeableContract :: Contract Parameter Storage
upgradeableContract = do
  unpair
  caseT @Parameter
    ( #cRun /-> do
        dip $ do
          getField #dataMap
          dip $ getField #code
        pair
        exec
        unpair
        dip $ setField #dataMap
        pair
    , #cUpgrade /-> do
        checkVersion
        bumpVersion
        migrateStorage
        toField #newCode
        setField #code
        nil; pair;
    , #cGetVersion /-> view_ (do cdr; toField #currentVersion)
    )

checkVersion :: '[UpgradeParameters, Storage] :-> '[UpgradeParameters, Storage]
checkVersion = do
  duupX @2; toField #currentVersion
  push @Natural 1
  add
  dip (getField #newVersion)
  stackType @('[Natural, Natural, UpgradeParameters, Storage])
  pair
  assertVersionsEqual
  where
    assertVersionsEqual = do
      dup
      unpair
      if IsEq
      then drop
      else do
        unpair
        toNamed #expected
        dip $ toNamed #actual
        pair
        userFail #cVersionMismatch

bumpVersion :: '[UpgradeParameters, Storage] :-> '[UpgradeParameters, Storage]
bumpVersion = do
  getField #newVersion
  swap
  dip $ setField #currentVersion

migrateStorage
  :: '[UpgradeParameters, Storage] :-> '[UpgradeParameters, Storage]
migrateStorage = do
  dip $ getField #dataMap
  getField #migrationScript
  swap
  dip $ do
    swap
    exec
    setField #dataMap
