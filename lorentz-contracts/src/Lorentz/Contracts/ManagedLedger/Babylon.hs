-- | Managed ledger which is compatible with FA1.2 standard and
-- extended with administrator functionality.
-- It is __not__ compatible with the Athens version of FA1.2.

module Lorentz.Contracts.ManagedLedger.Babylon
  ( Parameter (..)

  , Storage
  , Error (..)
  , mkStorage
  , managedLedgerContract

  , module Lorentz.Contracts.ManagedLedger.Types
  ) where

import Lorentz
import Prelude (sum)

import Lorentz.Contracts.ManagedLedger.Types
import Lorentz.Contracts.ManagedLedger.Impl

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

data Parameter
  = Transfer         !TransferParams
  | Approve          !ApproveParams
  | GetAllowance     !(View GetAllowanceParams Natural)
  | GetBalance       !(View Address Natural)
  | GetTotalSupply   !(View () Natural)
  | SetPause         !Bool
  | SetAdministrator !Address
  | GetAdministrator !(View () Address)
  | Mint             !MintParams
  | Burn             !BurnParams
  deriving stock Generic
  deriving anyclass IsoValue

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  } deriving stock Generic
    deriving anyclass IsoValue

type Storage = Storage' StorageFields

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = mkStorage' balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  }

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

managedLedgerContract :: Contract Parameter Storage
managedLedgerContract = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> transfer
    , #cApprove /-> approve
    , #cGetAllowance /-> getAllowance
    , #cGetBalance /-> getBalance
    , #cGetTotalSupply /-> getTotalSupply
    , #cSetPause /-> setPause
    , #cSetAdministrator /-> setAdministrator
    , #cGetAdministrator /-> getAdministrator
    , #cMint /-> mint
    , #cBurn /-> burn
    )
