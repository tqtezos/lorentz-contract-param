-- | Managed ledger which is compatible with FA1.2 standard and
-- extended with manager functionality.

module Lorentz.Contracts.ManagedLedger
  ( Parameter (..)
  , SaneParameter (..)
  , fromSaneParameter

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

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

-- We have to manually assemble linear parameter type, because Lorentz
-- uses balanced trees by default.

data Parameter0
  = Approve        !ApproveParams
  | Parameter1     !Parameter1
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter1
  = GetBalance     !(View Address Natural)
  | Parameter2     !Parameter2
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter2
  = GetAllowance   !(View GetAllowanceParams Natural)
  | Parameter3     !Parameter3
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter3
  = GetTotalSupply !(View () Natural)
  | Parameter4     !Parameter4
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter4
  = SetPause       !Bool
  | Parameter5     !Parameter5
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter5
  = SetManager     !Address
  | Parameter6     !Parameter6
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter6
  = GetManager     !(View () Address)
  | Parameter7     !Parameter7
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter7
  = Mint          !MintParams
  | Burn          !BurnParams
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter
  = Transfer       !TransferParams
  | Parameter0     !Parameter0
  deriving stock Generic
  deriving anyclass IsoValue

-- | Sane representation of parameter which we currently can't use
-- because it's Michelson representation is a balanced tree, while we
-- need to be compatible with FA1.2 which requires linear structure.
data SaneParameter
  = STransfer       !TransferParams
  | SMint           !MintParams
  | SBurn           !BurnParams
  | SApprove        !ApproveParams
  | SSetPause       !Bool
  | SSetManager     !Address
  | SGetManager     !(View () Address)
  | SGetAllowance   !(View GetAllowanceParams Natural)
  | SGetTotalSupply !(View () Natural)
  | SGetBalance     !(View Address Natural)

-- | Convert sane parameter to parameter of this contract.
fromSaneParameter :: SaneParameter -> Parameter
fromSaneParameter =
  \case
    STransfer tp -> Transfer tp
    SMint mp ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      Parameter3 $
      Parameter4 $
      Parameter5 $
      Parameter6 $
      Parameter7 $
      Mint mp
    SBurn mp ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      Parameter3 $
      Parameter4 $
      Parameter5 $
      Parameter6 $
      Parameter7 $
      Burn mp
    SApprove ap ->
      Parameter0 $
      Approve ap
    SSetPause b ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      Parameter3 $
      Parameter4 $
      SetPause b
    SSetManager a ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      Parameter3 $
      Parameter4 $
      Parameter5 $
      SetManager a
    SGetManager v ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      Parameter3 $
      Parameter4 $
      Parameter5 $
      Parameter6 $
      GetManager v
    SGetAllowance v ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      GetAllowance v
    SGetTotalSupply v ->
      Parameter0 $
      Parameter1 $
      Parameter2 $
      Parameter3 $
      GetTotalSupply v
    SGetBalance v ->
      Parameter0 $
      Parameter1 $
      GetBalance v

type TransferParams = ("from" :! Address, "to" :! Address, "deltaVal" :! Natural)
type MintParams = ("to" :! Address, "deltaVal" :! Natural)
type BurnParams = ("from" :! Address, "deltaVal" :! Natural)
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
  | SenderIsNotManager
    -- ^ Contract initiator has not enough rights to perform this operation.
  | NotEnoughBalance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient balance.
  | NotEnoughAllowance ("required" :! Natural, "present" :! Natural)
    -- ^ Insufficient allowance to transfer foreign funds.
  | OperationsArePaused
    -- ^ Operation is unavailable until resume by token manager.
  deriving stock (Eq, Generic)

deriveCustomError ''Error

instance Buildable Error where
  build = \case
    UnsafeAllowanceChange prevVal ->
      "Cannot change allowance from " +| prevVal |+ " to a non-zero value"
    SenderIsNotManager ->
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
    , #cParameter0 /-> caseT @Parameter0
      ( #cApprove /-> do
          dip ensureNotPaused
          approveParamsToAllowanceParams
          dupT @Storage; dupT @AllowanceParams; allowance
          dup; int
          if IsZero
            then drop
            else do duupX @2; toField #val; int
                    ifEq0 drop (userFail #cUnsafeAllowanceChange)
          setAllowance; nil; pair
      , #cParameter1 /-> caseT @Parameter1
        ( #cGetBalance /-> view_ $ do
            unpair; dip ( toField #ledger ); get;
            ifSome (toField #balance) (push 0)
        , #cParameter2 /-> caseT @Parameter2
          ( #cGetAllowance /-> view_ (do unpair; allowance)
          , #cParameter3 /-> caseT @Parameter3
            ( #cGetTotalSupply /->
              view_ (do cdr; toField #fields; toField #totalSupply)
            , #cParameter4 /-> caseT @Parameter4
              ( #cSetPause /-> do
                  dip authorizeManager
                  dip (getField #fields); setField #paused; setField #fields
                  nil; pair
              , #cParameter5 /-> caseT @Parameter5
                ( #cSetManager /-> do
                    dip authorizeManager;
                    stackType @[Address, Storage]
                    dip (getField #fields); setField #manager; setField #fields
                    nil; pair;
                , #cParameter6 /-> caseT @Parameter6
                  ( #cGetManager /->
                      view_ (do cdr; toField #fields; toField #manager)
                  , #cParameter7 /-> caseT @Parameter7
                      ( #cMint /-> do
                          dip authorizeManager
                          creditTo
                          drop @MintParams
                          nil; pair
                      , #cBurn /-> do
                          dip authorizeManager
                          debitFrom
                          drop @BurnParams
                          nil; pair
                      )
                  )
                )
              )
            )
          )
        )
      )
    )

userFail :: forall name fieldTy s s'. FailUsingArg Error name fieldTy s s'
userFail = failUsingArg @Error @name

authorizeManager :: Storage : s :-> Storage : s
authorizeManager = do
  getField #fields; toField #manager; sender; eq
  if_ nop (failUsing SenderIsNotManager)

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
