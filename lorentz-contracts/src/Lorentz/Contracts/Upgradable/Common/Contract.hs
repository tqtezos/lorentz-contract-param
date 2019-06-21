module Lorentz.Contracts.Upgradable.Common.Contract
  ( Parameter(..)
  , Storage(..)
  , upgradableContract
  , emptyMigration
  ) where

import Lorentz

import qualified Data.Map as M

import qualified Michelson.Typed as T

import Lorentz.Contracts.Upgradable.Common.Base

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

upgradableContract :: Contract Parameter Storage
upgradableContract = do
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
