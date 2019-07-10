module Lorentz.Contracts.Upgradeable.Common.Contract
  ( Parameter(..)
  , Storage(..)
  , upgradeableContract
  , emptyMigration
  ) where

import Lorentz

import qualified Data.Map as M

import qualified Michelson.Typed as T

import Lorentz.Contracts.Upgradeable.Common.Base

data Parameter
  = Run UParameter
  | Upgrade (MigrationScript, ContractCode)
  deriving stock Generic
  deriving anyclass IsoValue

data Storage = Storage
  { dataMap :: UStore_
  , code :: ContractCode
  }
  deriving stock Generic
  deriving anyclass IsoValue

emptyCode :: ContractCode
emptyCode = unpair # drop # nil # pair

emptyMigration :: Storage
emptyMigration = Storage
  { dataMap = T.BigMap $ M.fromList []
  , code = emptyCode
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
        dip $ getField #dataMap
        unpair
        swap
        dip $ do
          swap
          exec
          setField #dataMap
        setField #code;
        nil; pair;
    )
