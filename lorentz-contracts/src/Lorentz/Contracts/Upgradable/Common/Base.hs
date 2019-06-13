module Lorentz.Contracts.Upgradable.Common.Base
  ( UParameter
  , UStorage
  , MigrationScript
  , ContractCode
  ) where

import Lorentz

type UParameter = (MText, ByteString)
type UStorage = BigMap ByteString ByteString
type MigrationScript = Lambda UStorage UStorage
type ContractCode = Lambda (UParameter, UStorage) ([Operation], UStorage)
