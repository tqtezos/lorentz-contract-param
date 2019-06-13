-- | The implementation of Unsafe ledger with V1 balance bug fixed

module Lorentz.Contracts.UpgradableUnsafeLedger.V2
  ( migrate
  , unsafeLedgerContract
  ) where

import Lorentz

import Lorentz.Contracts.Upgradable.Common
import qualified Lorentz.Contracts.UpgradableUnsafeLedger.V1 as V1

-- Since the storage is the same, there's nothing to migrate
migrate :: '[UStorage] :-> '[UStorage]
migrate = nop

-- Most of the functions are from V1 except for getBalance
unsafeLedgerContract :: ContractCode
unsafeLedgerContract = do
  dispatch
    [ ifArg @V1.TransferParams [mt|Transfer|] V1.transfer
    , ifArg @(View () Natural) [mt|GetTotalSupply|] V1.getTotalSupply
    , ifArg @(View Address (Maybe Natural)) [mt|GetBalance|] getBalance
    ]

-- The new getBalance returns the correct balance
getBalance :: '[View Address (Maybe Natural), UStorage]
           :-> '[([Operation], UStorage)]
getBalance = view_ (do unpair; V1.toElementOfBigMap_ledger)
