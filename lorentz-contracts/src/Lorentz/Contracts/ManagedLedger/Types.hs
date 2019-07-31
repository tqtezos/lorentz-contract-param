-- | Types shared between contracts participating in ManagedLedger.

module Lorentz.Contracts.ManagedLedger.Types
  ( TransferParams
  , ApproveParams
  , AllowanceParams
  , GetAllowanceParams
  , MintParams
  , BurnParams

  , LedgerValue
  , Error (..)
  , Storage' (..)
  , mkStorage'
  ) where

import Lorentz
import Prelude ((<$>))

import Fmt (Buildable(..), (+|), (|+))

import Util.Named ((.!))

type TransferParams = ("from" :! Address, "to" :! Address, "value" :! Natural)
type ApproveParams = ("spender" :! Address, "value" :! Natural)
type AllowanceParams = ("owner" :! Address, "spender" :! Address, "value" :! Natural)
type GetAllowanceParams = ("owner" :! Address, "spender" :! Address)
type MintParams = ("to" :! Address, "value" :! Natural)
type BurnParams = ("from" :! Address, "value" :! Natural)

type LedgerValue = ("balance" :! Natural, "approvals" :! Map Address Natural)

data Error
  = UnsafeAllowanceChange Natural
    -- ^ Attempt to change allowance from non-zero to a non-zero value.
  | SenderIsNotAdmin
    -- ^ Contract initiator has not enough rights to perform this operation.
  | NotEnoughBalance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient balance.
  | NotEnoughAllowance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient allowance to transfer foreign funds.
  | OperationsArePaused
    -- ^ Operation is unavailable until resume by token admin.
  deriving stock (Eq, Generic)

deriveCustomError ''Error

instance Buildable Error where
  build = \case
    UnsafeAllowanceChange prevVal ->
      "Cannot change allowance from " +| prevVal |+ " to a non-zero value"
    SenderIsNotAdmin ->
      "This operation can be executed only by admin, but is invoked by \
      \someone else"
    NotEnoughBalance (arg #required -> required, arg #present -> present) ->
      "Insufficient balance, needed " +| required |+ ", but only" +|
      present |+ " is present"
    NotEnoughAllowance (arg #required -> required, arg #present -> present) ->
      "Insufficient allowance, you can spend only " +| present |+ ", but " +|
      required |+ " was requested"
    OperationsArePaused ->
      "Operations are paused and cannot be invoked"

data Storage' fields = Storage'
  { ledger :: BigMap Address LedgerValue
  , fields :: fields
  } deriving stock Generic
    deriving anyclass IsoValue

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage' :: Map Address Natural -> fields -> Storage' fields
mkStorage' balances flds = Storage'
  { ledger = BigMap $ toLedgerValue <$> balances
  , fields = flds
  }
  where
    toLedgerValue initBal = (#balance .! initBal, #approvals .! mempty)
