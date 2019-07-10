module Lorentz.Contracts.Upgradeable.Common.Base
  ( UParameter
  , UStore_
  , MigrationScript
  , ContractCode
  ) where

import Lorentz

type UParameter = (MText, ByteString)
type UStore_ = BigMap ByteString ByteString
type MigrationScript = Lambda UStore_ UStore_
type ContractCode = Lambda (UParameter, UStore_) ([Operation], UStore_)
