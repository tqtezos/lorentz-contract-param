-- | FA 1.4: ERC-20 equivalent with Mintable, Burnable, Pausable,
-- Ownable functionality.
module Lorentz.Contracts.ManagedLedger
  ( Parameter (..)
  , Storage (..)
  , Error (..)
  , mkStorage
  , managedLedgerContract
  ) where

import Lorentz

import Fmt (Buildable (..), (+|), (|+))

import Util.Instances ()
import Util.Named ()

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Parameter
  = Transfer       TransferParams
  | Approve        ApproveParams
  | SetPause       Bool
  | SetManager     Address
  | GetManager     (View () Address)
  | GetAllowance   (View GetAllowanceParams Natural)
  | GetTotalSupply (View () Natural)
  | GetBalance     (View Address Natural)
  deriving stock Generic
  deriving anyclass IsoValue

type TransferParams = ("from" :! Address, "to" :! Address, "deltaVal" :! Natural)
type AllowanceParams = ("from" :! Address, "to" :! Address, "val" :! Natural)
type ApproveParams = ("to" :! Address, "val" :! Natural)
type GetAllowanceParams = ("from" :! Address, "to" :! Address)

data StorageFields = StorageFields
  { manager     :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  } deriving stock Generic
    deriving anyclass IsoValue

data Storage = Storage
  { ledger :: BigMap Address LedgerValue
  , fields :: StorageFields
  } deriving stock Generic
    deriving anyclass IsoValue

mkStorage :: Address -> Storage
mkStorage managerAddress = Storage
  { ledger = mempty
  , fields = StorageFields
    { manager = managerAddress
    , paused = False
    , totalSupply = 0
    }
  }

type LedgerValue = ("balance" :! Natural, "approvals" :! Map Address Natural)

data Error
  = UnsafeAllowanceChange Natural
    -- ^ Attempt to change allowance from non-zero to a non-zero value.
  | InitiatorIsNotManager
    -- ^ Contract initiator has not enough rights to perform this operation.
  | NotEnoughBalance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient balance.
  | NotEnoughAllowance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient allowance to transfer foreign funds.
  | OperationsArePaused
    -- ^ Operation is unavailable until resume by token manager.
  | ManagerAddressWouldShadow
    -- ^ Cannot set token manager to the given address because it already
    -- appears in the ledger.
  deriving stock (Eq, Generic)

deriveCustomError ''Error

instance Buildable Error where
  build = \case
    UnsafeAllowanceChange prevVal ->
      "Cannot change allowance from " +| prevVal |+ " to a non-zero value"
    InitiatorIsNotManager ->
      "This operation can be executed only by manager, but is invoked by \
      \someone else"
    NotEnoughBalance (arg #required -> required, arg #present -> present) ->
      "Insufficient balance, needed " +| required |+ ", but only" +|
      present |+ " is present"
    NotEnoughAllowance (arg #required -> required, arg #present -> present) ->
      "Insufficient allowance, you can spend only " +| present |+ ", but " +|
      required |+ " was requested"
    OperationsArePaused ->
      "Operations are paused and cannot be invoked"
    ManagerAddressWouldShadow ->
      "This address cannot be assigned as new manager because it appears \
      \in the ledger"

managedLedgerContract :: Contract Parameter Storage
managedLedgerContract = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> do
        dip ensureNotPaused
        getField #to; dip (getField #from)
        if IsEq
          then drop
          else do
            stackType @[TransferParams, Storage]
            getFieldNamed #from; dip (dupT @Storage); needAllowance
            if_ (do dup; dip @TransferParams consumeAllowance) nop
            creditTo; debitFrom
            drop @TransferParams
        nil; pair
    , #cApprove /-> do
        dip ensureNotPaused
        approveParamsToAllowanceParams
        dupT @Storage; dupT @AllowanceParams; allowance
        dup; int
        if IsZero
          then drop
          else do duupX @2; toField #val; int
                  ifEq0 drop (userFail #cUnsafeAllowanceChange)
        setAllowance; nil; pair
    , #cSetPause /-> do
        dip authorizeManager
        dip (getField #fields); setField #paused; setField #fields
        nil; pair
    , #cSetManager /-> do
        dip authorizeManager;
        dupT @Storage; toField #ledger; dupT @Address; mem
        if_ (failUsing ManagerAddressWouldShadow) nop
        stackType @[Address, Storage]
        dip (getField #fields); setField #manager; setField #fields
        nil; pair;
    , #cGetManager /-> do view_ (do cdr; toField #fields; toField #manager)
    , #cGetAllowance /-> view_ (do unpair; allowance)
    , #cGetTotalSupply /-> do view_ (do cdr; toField #fields; toField #totalSupply)
    , #cGetBalance /-> view_ $ do
        unpair; dip ( toField #ledger ); get;
        ifSome (toField #balance) (push 0)
    )

userFail :: forall name fieldTy s s'. FailUsingArg Error name fieldTy s s'
userFail = failUsingArg @Error @name

authorizeManager :: Storage : s :-> Storage : s
authorizeManager = do
  getField #fields; toField #manager; sender; eq
  if_ nop (failUsing InitiatorIsNotManager)

addTotalSupply :: Integer : Storage : s :-> Storage : s
addTotalSupply = do
  dip $ getField #fields >> getField #totalSupply
  add; isNat; ifSome nop (failUnexpected [mt|Negative total supply|])
  setField #totalSupply; setField #fields

debitFrom
  :: forall param.
     (param `HasFieldsOfType` ["from" := Address, "deltaVal" := Natural])
  => '[param, Storage] :-> '[param, Storage]
debitFrom = do
  -- Pass if debiting from manager
  getField #from; duupX @3; toField #fields; toField #manager
  if IsEq
  then nop
  else do
    -- Get LedgerValue
    duupX @2; toField #ledger; duupX @2; toField #from
    get; ifSome nop $ do
      -- Fail if absent
      stackType @[param, Storage]
      toField #deltaVal; toNamed #required; push 0; toNamed #present
      swap; pair; userFail #cNotEnoughBalance
    -- Get balance
    stackType @[LedgerValue, param, Storage]
    getField #balance
    duupX @3; toField #deltaVal
    rsub; isNat
    ifSome nop $ do
      -- Fail if balance is not enough
      stackType @[LedgerValue, param, Storage]
      toField #balance; toNamed #present
      duupX @2; toField #deltaVal; toNamed #required
      pair; userFail #cNotEnoughBalance
    -- Update balance, LedgerValue and Storage
    setField #balance;
    duupX @2; dip $ do
      nonEmptyLedgerValue; swap; toField #from
      dip (dip $ getField #ledger)
      update; setField #ledger

    -- Update total supply
    dup; dip $ do toField #deltaVal; neg; addTotalSupply

creditTo
  :: (param `HasFieldsOfType` ["to" := Address, "deltaVal" := Natural])
  => '[param, Storage] :-> '[param, Storage]
creditTo = do
  -- Pass if crediting to manager
  getField #to; duupX @3; toField #fields; toField #manager
  if IsEq
  then nop
  else do
    -- Get LedgerValue
    duupX @2; toField #ledger; duupX @2; toField #to
    get
    if IsSome
      then do -- Get balance
              duupX @2; toField #deltaVal; dip (getField #balance)
              add @Natural; setField #balance; some
      else do -- Construct LedgerValue (if not empty)
              getField #deltaVal; int
              ifEq0 none $ do
                constructT @LedgerValue
                  ( fieldCtor $ getField #deltaVal >> toNamed #balance
                  , fieldCtor $ emptyMap >> toNamed #approvals
                  )
                some
    -- Update LedgerValue and Storage
    swap
    dipX @2 (getField #ledger)
    dup; dip $ do toField #to; update; setField #ledger

    -- Update total supply
    dup; dip $ do toField #deltaVal; int; addTotalSupply

emptyLedgerValue :: s :-> LedgerValue : s
emptyLedgerValue =
  constructT @LedgerValue
    ( fieldCtor $ push 0 >> coerceWrap
    , fieldCtor $ emptyMap >> coerceWrap
    )

-- | Ensure that given 'LedgerValue' value cannot be safely removed
-- and return it.
nonEmptyLedgerValue :: LedgerValue : s :-> Maybe LedgerValue : s
nonEmptyLedgerValue = do
  getField #balance; int
  if IsNotZero
  then some
  else do
    getField #approvals
    size; int
    if IsNotZero
    then some
    else drop >> none

approveParamsToAllowanceParams :: ApproveParams : s :-> AllowanceParams : s
approveParamsToAllowanceParams = do
  -- a hack for performance
  -- can be replaced with 'constructT' call if stops working
  sender; toNamed #from; pair; coerce_

allowance
  :: (param `HasFieldsOfType` ["from" := Address, "to" := Address])
  => param ': Storage ': s :-> Natural ': s
allowance = do
  dup; dip $ do
    toField #from; dip (toField #ledger); get;
    ifSome (toField #approvals) emptyMap
  toField #to; get; ifNone (push 0) nop

setAllowance :: (AllowanceParams ': Storage ': s)
             :-> (Storage ': s)
setAllowance = do
  dip (getField #ledger); swap;
  dip (getField #from); swap;
  get; ifNone (emptyLedgerValue >> emptyMap) (getField #approvals);
  stackType @(Map Address Natural : LedgerValue : AllowanceParams : Storage : _)
  duupX @3; toField #val; nonZero
  duupX @4; toField #to
  update; setField #approvals
  stackType @(LedgerValue : AllowanceParams : Storage : _)
  some; swap
  toField #from; dip (dip $ getField #ledger)
  update
  setField #ledger

consumeAllowance
  :: forall param s.
     (param `HasFieldsOfType` ["from" := Address, "deltaVal" := Natural])
  => (param ': Storage ': s) :-> (Storage ': s)
consumeAllowance = do
  dup; dip @param $ do
    dip (dup @Storage)
    toFieldNamed #from; sender; toNamed #to; pair
    allowance
  stackType @(param : Natural : Storage : s)
  constructT @AllowanceParams
    ( fieldCtor $ getFieldNamed #from
    , fieldCtor $ sender >> toNamed #to
    , fieldCtor $ do
        getField #deltaVal; duupX @3
        sub; isNat;
        if IsSome
          then nop
          else do duupX @2; toNamed #present
                  duupX @2; toField #deltaVal; toNamed #required
                  pair; userFail #cNotEnoughAllowance
        toNamed #val
    )
  dip $ drop @param >> drop @Natural
  setAllowance

-- | Whether do we need to take allowance into account
-- when performing given funds transfer.
needAllowance :: ("from" :! Address) : Storage : s :-> Bool : s
needAllowance = do
  fromNamed #from
  sender
  if IsEq
  then drop >> push False
  else do toField #fields; toField #manager; sender; neq

ensureNotPaused :: Storage : s :-> Storage : s
ensureNotPaused = do
  getField #fields; toField #paused
  if_ (failUsing OperationsArePaused) nop
