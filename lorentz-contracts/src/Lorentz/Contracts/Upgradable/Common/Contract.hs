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
  | Upgrade ContractCode
  deriving stock Generic
  deriving anyclass IsoValue

data Storage = Storage
  { dataMap :: UStorage
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
    ( #cRun /-> do -- UParameter : Storage
        dip $ do   -- Storage
          getField #dataMap    -- UStorage : Storage
          dip $ getField #code -- UStorage : ContractCode : Storage
        -- UParameter : UStorage : ContractCode : Storage
        pair   -- (UParameter, UStorage) : ContractCode : Storage
        exec   -- ([Operation], UStorage) : Storage
        unpair -- [Operation] : UStorage : Storage
        dip $ setField #dataMap -- [Operation] : Storage
        pair   -- ([Operation], Storage)
    , #cUpgrade /-> do
        setField #code;
        nil; pair;
    )
