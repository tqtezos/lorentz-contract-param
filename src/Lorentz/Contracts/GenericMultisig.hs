{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

-- | To use the contract:
-- @
--  stack build
--  stack exec -- lorentz-contracts print --name MultisigManagedLedgerAthens -o MultisigManagedLedgerAthens.tz
--  stack exec -- lorentz-contracts print --name ExplicitBigMapManagedLedgerAthens -o ExplicitBigMapManagedLedgerAthens.tz
--
--  alias alpha-client="tezos-client -A rpcalpha.tzbeta.net -P 443 -S"
--  alpha-client activate account fred with /Users/michaelklein/Downloads/tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir.json
--  alpha-client get balance for fred
--  FRED_ADDRESS="tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir"
--  _
--
-- @
module Lorentz.Contracts.GenericMultisig where

import Lorentz hiding (concat)
import Lorentz.Contracts.Util ()
import Michelson.Typed.Scope
import qualified Tezos.Crypto as Crypto
import qualified Tezos.Crypto.Ed25519 as Ed25519

import Fmt (Buildable(..), (+|), (|+))
import Data.ByteString.Internal (unpackChars)
import Data.Aeson hiding (String)

import Data.Bifunctor
import Data.String
import Text.Read
import Text.Show (Show(..))
import Data.Bool
import Data.List (unwords, concat)
import Data.Kind
import Data.Typeable
import qualified GHC.Base as Base
import qualified Control.Monad as Monad

-- {-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Some `Public` key
data SomePublicKey where
  SomePublicKey :: forall key. IsKey key => Proxy key -> Public key -> SomePublicKey

-- | Keys have "value-like" instances, e.g. `Typeable`, `Ord`, `Show`, `IsoValue`, etc.,
-- public keys, partial and total signatures, and ways to make and check signatures
class ( Typeable key
      , Ord (Public key)
      , Read (Public key)
      , Show (Public key)
      , ToJSON (Public key)
      , ToJSONKey (Public key)
      , FromJSON (Public key)
      , FromJSONKey (Public key)
      , IsoValue (Public key)
      , KnownValue (Public key)
      , NiceParameter (Public key)
      , HasNoOp (ToT (Public key))
      , HasNoBigMap (ToT (Public key))
      , HasNoNestedBigMaps (ToT (Public key))
      , Eq (PartialSig key)
      , Show (PartialSig key)
      , ToJSON (PartialSig key)
      , FromJSON (PartialSig key)
      , Eq (Sig key)
      , Read (Sig key)
      , Show (Sig key)
      , FromJSON (Sig key)
      , ToJSON (Sig key)
      , IsoValue (Sig key)
      , KnownValue (Sig key)
      , NiceParameter (Sig key)
      , HasNoOp (ToT (Sig key))
      , HasNoBigMap (ToT (Sig key))
      , HasNoNestedBigMaps (ToT (Sig key))
      ) => IsKey (key :: Type) where
  -- | The public key
  type Public key :: Type
  -- | A partial signature
  type PartialSig key :: Type
  -- | A complete signature
  type Sig key :: Type

  -- | Check the signature in Michelson
  checkKeySignature :: forall s. (Public key & (Sig key & (ByteString & s))) :-> (Bool & s)
  -- | Check the signature in Haskell
  checkKeySignatureHaskell :: Public key -> Sig key -> ByteString -> Either String ()

  -- | An empty `PartialSig`
  partialSig :: PartialSig key
  -- | Sign a `PartialSig` with the given `Ed25519.SecretKey`
  signWithKey :: Ed25519.SecretKey -> ByteString -> PartialSig key -> Either Base.String (PartialSig key)
  -- | Complete a `PartialSig` or return an error explaining how it's incomplete
  completeSig :: PartialSig key -> Either Base.String (Sig key)


instance IsKey PublicKey where
  type Public PublicKey = PublicKey
  type PartialSig PublicKey = Maybe Signature
  type Sig PublicKey = Signature
  checkKeySignature = checkSignature
  checkKeySignatureHaskell publicKey sig bytes' =
    bool
       (Left "Checking the signature failed")
       (return ()) $
       Crypto.checkSignature publicKey sig bytes'

  partialSig = Nothing

  signWithKey secretKey bytes Nothing =
    let signature = Ed25519.sign secretKey bytes -- Crypto.SignatureEd25519 $
     in let publicKey = Ed25519.toPublic secretKey
         in bool
              (Left $ "Failed to check signature: " Base.++ show signature)
              (Right $ Just $ Crypto.SignatureEd25519 signature)
              (Ed25519.checkSignature publicKey signature bytes)
  signWithKey _ _ (Just sig) =
    Left $ "signWithKey @PublicKey: already signed " Base.++ show sig

  completeSig Nothing = Left "incomplete"
  completeSig (Just sig) = Right sig

-- | A pair of partial signatures
data PartialSigPair a b =
    EmptySigPair
  | PartialSigL (PartialSig a)
  | PartialSigLR (Sig a) (PartialSig b)
  | PartialSigR (PartialSig b)
  | PartialSigRL (PartialSig a) (Sig b)
  | SigPair (Sig a) (Sig b)
  deriving (Generic)

deriving instance (IsKey a, IsKey b) => Eq (PartialSigPair a b)
deriving instance (IsKey a, IsKey b) => Show (PartialSigPair a b)
deriving instance (IsKey a, IsKey b) => FromJSON (PartialSigPair a b)
deriving instance (IsKey a, IsKey b) => ToJSON (PartialSigPair a b)

instance (IsKey a, IsKey b) => IsKey (a, b) where
  type Public (a, b) = (Public a, Public b)
  -- type Secret (a, b) = (Secret a, Secret b)
  type PartialSig (a, b) = PartialSigPair a b
  type Sig (a, b) = (Sig a, Sig b)
  checkKeySignature = do
    pair
    dup
    dip $ do
      unpair
      car
      dip $ do
        car
        dip dup
      checkKeySignature @a
    swap
    dip $ do
      unpair
      cdr
      dip cdr
      checkKeySignature @b
    and

  checkKeySignatureHaskell ~(publicKeyA, publicKeyB) ~(sigA, sigB) bytes' =
    (first (\str -> concat [show publicKeyA, ": ", str]) $ checkKeySignatureHaskell @a publicKeyA sigA bytes') Monad.>>
    (first (\str -> concat [show publicKeyB, ": ", str]) $ checkKeySignatureHaskell @b publicKeyB sigB bytes')

  partialSig = EmptySigPair

  signWithKey secretKey bytes EmptySigPair =
    case signWithKey @a secretKey bytes $ partialSig @a of
      Left errA ->
        case signWithKey @b secretKey bytes $ partialSig @b of
          Left errB -> Left $ "Neither key worked: " <> unwords [errA, errB]
          Right psigB ->
            case completeSig @b psigB of
              Left _ ->
                return $ PartialSigR @a @b psigB
              Right sigB ->
                return $ PartialSigRL @a @b (partialSig @a) sigB
      Right psigA ->
        case completeSig @a psigA of
          Left _ ->
            return $ PartialSigL @a @b psigA
          Right sigA ->
            return $ PartialSigLR @a @b sigA (partialSig @b)
  signWithKey secretKey bytes (PartialSigL psigA) =
    case signWithKey @a secretKey bytes psigA of
      Left err -> Left err
      Right psigA' ->
        case completeSig @a psigA' of
          Left _ ->
            return $ PartialSigL @a @b psigA'
          Right sigA ->
            return $ PartialSigLR @a @b sigA (partialSig @b)
  signWithKey secretKey bytes (PartialSigR psigB) =
    case signWithKey @b secretKey bytes psigB of
      Left err -> Left err
      Right psigB' ->
        case completeSig @b psigB' of
          Left _ ->
            return $ PartialSigR @a @b psigB'
          Right sigB ->
            return $ PartialSigRL @a @b (partialSig @a) sigB
  signWithKey secretKey bytes (PartialSigLR sigA psigB) =
    case signWithKey @b secretKey bytes psigB of
      Left err -> Left err
      Right psigB' ->
        case completeSig @b psigB' of
          Left _ ->
            return $ PartialSigLR @a @b sigA psigB'
          Right sigB ->
            return $ SigPair @a @b sigA sigB
  signWithKey secretKey bytes (PartialSigRL psigA sigB) =
    case signWithKey @a secretKey bytes psigA of
      Left err -> Left err
      Right psigA' ->
        case completeSig @a psigA' of
          Left _ ->
            return $ PartialSigRL @a @b psigA' sigB
          Right sigA ->
            return $ SigPair @a @b sigA sigB
  signWithKey _ _ (SigPair sigA sigB) =
    Left $ "signWithKey @(a, b): all signatures have been provided: " Base.++ show (sigA, sigB)

  completeSig EmptySigPair = Left "incomplete: EmptySigPair"
  completeSig (PartialSigL _) = Left "incomplete: PartialSigL"
  completeSig (PartialSigR _) = Left "incomplete: PartialSigR"
  completeSig (PartialSigLR _ _) = Left "incomplete: PartialSigLR"
  completeSig (PartialSigRL _ _) = Left "incomplete: PartialSigRL"
  completeSig (SigPair sigA sigB) = Right (sigA, sigB)


----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

-- parameter (or (unit %default)
--               (pair %main
--                  (pair :payload
--                     (nat %counter) # counter, used to prevent replay attacks
--                     (or :action    # payload to sign, represents the requested action
--                        (lambda %operation unit (list operation))
--                        (pair %change_keys          # change the keys controlling the multisig
--                           (nat %threshold)         # new threshold
--                           (list %keys key))))     # new list of keys
--                  (list %sigs (option signature))));    # signatures

-- | @(threshold, keys)@
--
-- Note: @threshold@ is also known as @quorum@
type ChangeKeyParams key =
  ( Natural     -- "threshold" :!
  , [Public key] -- "keys"      :!
  )


-- | Either perform an `Operation` with the included contract or
-- use `ChangeKeys` to update the key list and threshold (quorum)
-- @
--  type GenericMultisigAction a = Either a ChangeKeyParams
-- @
data GenericMultisigAction key a
  = Operation !a
  | ChangeKeys !(ChangeKeyParams key)
  deriving stock Generic

deriving instance (IsKey key, Read a) => Read (GenericMultisigAction key a)
deriving instance (IsKey key, Show a) => Show (GenericMultisigAction key a)
deriving instance (IsKey key, IsoValue a) => IsoValue (GenericMultisigAction key a)


-- | @((counter, action), sigs)@
--
-- @
--  data MainParams = MainParams
--    { counter :: !Natural
--    , action  :: !GenericMultisigAction
--    , sigs    :: ![Maybe Signature]
--    }
--    deriving stock Generic
--    deriving anyclass IsoValue
-- @
type MainParams key a =
  ( ( Natural                 -- "counter" :!
    , GenericMultisigAction key a  -- "action"  :!
    )
  , [Maybe (Sig key)]       -- "sigs"    :!
  )

-- | Use `Default` to send tokens to the contract.
-- Otherwise, use `MainParameter`
data Parameter key a
  = Default
  | MainParameter (MainParams key a)
  deriving stock Generic

deriving instance (IsKey key, Read a) => Read (Parameter key a)
deriving instance (IsKey key, Show a) => Show (Parameter key a)
deriving instance (IsKey key, IsoValue a) => IsoValue (Parameter key a)


----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

-- storage (pair (nat %stored_counter) (pair (nat %threshold) (list %keys key))) ;

-- | @(storedCounter, (threshold, keys))@
--
-- @
--  data Storage = Storage
--    { storedCounter :: !Natural
--    , threshold     :: !Natural
--    , keys          :: ![PublicKey]
--    } deriving stock Generic
--      deriving anyclass IsoValue
-- @
type Storage key =
  ( Natural -- "storedCounter" :!
  , ( Natural     -- "threshold" :!
    , [Public key] -- "keys"      :!
    )
  )


-- -- | Errors specific to this implementation.
-- data GenericMultisigError
--   = InvalidSignature
--   | FewerSignaturesThanKeys
--   | UncheckedSignaturesRemain
--   deriving stock (Eq, Generic)
--
-- deriveCustomError ''GenericMultisigError

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

-- | Attempt to change allowance from non-zero to a non-zero value.

-- | A provided `Signature` is invalid
type instance ErrorArg "invalidSignature" = ByteString

-- | There are fewer signatures than keys in the given parameters
type instance ErrorArg "fewerSignaturesThanKeys" = PublicKey
  -- ("required" :! Natural, "present" :! Natural)

-- | Unchecked signatures remain
type instance ErrorArg "uncheckedSignaturesRemain" = Maybe Signature

-- Buildable instances
----------------------------------------------------------------------------

instance Buildable ByteString where
  build = build . unpackChars

instance Buildable (CustomError "invalidSignature") where
  build (CustomError _ invalidSignature) =
    "Invalid signature: " +| invalidSignature |+ ""

instance Buildable (CustomError "fewerSignaturesThanKeys") where
  build (CustomError _ leftoverKey) =
    "Fewer signatures than keys: " +| leftoverKey |+ " is left over"
  -- build (CustomError _ (arg #required -> required, arg #present -> present)) =
  --   "Fewer signatures than keys, needed " +| required |+ ", but only" +|
  --   present |+ " is present"

instance Buildable (CustomError "uncheckedSignaturesRemain") where
  build (CustomError _ uncheckedSignatures) =
    "Unchecked signatures remain: " +| uncheckedSignatures |+ ""

-- Documentation
----------------------------------------------------------------------------

instance CustomErrorHasDoc "invalidSignature" where
  customErrDocMdCause =
    "A signature has been provided that does not match both the expected public \
    \key and data to be signed."
  customErrClass = ErrClassBadArgument

instance CustomErrorHasDoc "fewerSignaturesThanKeys" where
  customErrDocMdCause =
    "Fewer signatures than known public keys are present"
  customErrClass = ErrClassBadArgument

instance CustomErrorHasDoc "uncheckedSignaturesRemain" where
  customErrDocMdCause =
    "Unchecked signatures remain after validation"
  customErrClass = ErrClassContractInternal

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

-- | The default action: do nothing
genericMultisigContractDefault :: (b & s) :-> (([Operation], b) & s)
genericMultisigContractDefault = do
  nil @Operation
  pair

-- | Assert no token was sent:
-- to send tokens, the default entry point should be used
assertNoTokensSent :: s :-> s
assertNoTokensSent = do
  --   # Assert no token was sent:
  --   # to send tokens, the default entry point should be used
  --   PUSH mutez 0 ; AMOUNT ; ASSERT_CMPEQ ;
  let tokensSentOutsideDefault = [mt|Some tokens were sent to this contract outside of the default entry point.|]
  push (toMutez 0 :: Mutez) >> amount >> assertEq tokensSentOutsideDefault


-- | Pair the payload with the current contract address, to ensure signatures
-- | can't be replayed accross different contracts if a key is reused.
preparePayload :: forall key a b c s p. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
  => Proxy p
  -> (((Natural, GenericMultisigAction key a), b) & (c & s)) :-> (c & (Natural & (ByteString & (b & (GenericMultisigAction key a & (c & s))))))
preparePayload _ = do
  --   SWAP ; DUP ; DIP { SWAP } ;
  swap >> dup >> dip swap

  -- DIP
  --   {
  --     UNPAIR ;
  --     # pair the payload with the current contract address, to ensure signatures
  --     # can't be replayed accross different contracts if a key is reused.
  --     DUP ; SELF ; ADDRESS ; PAIR ;
  --     PACK ; # form the binary payload that we expect to be signed
  --     DIP { UNPAIR @counter ; DIP { SWAP } } ; SWAP
  --   } ;
  dip $ do
    unpair
    dup >> self @p >> address >> pair
    pack
    dip (unpair @Natural >> dip swap) >> swap

-- | `assertEq` on the parameter counter and storage counter
checkCountersMatch :: ((Natural, b) & (Natural & s)) :-> (b & s)
checkCountersMatch = do
  -- # Check that the counters match
  -- UNPAIR @stored_counter; DIP { SWAP };
  -- ASSERT_CMPEQ ;
  unpair >> dip swap
  -- getField #storedCounter
  let countersDoNotMatch = [mt|Counters do not match.|]
  assertEq countersDoNotMatch


-- | Compute the number of valid signatures
countValidSignatures :: forall key a s. IsKey key =>
     ((a, [Public key]) & (ByteString & (List (Maybe (Sig key)) & s))) :-> (a & (Natural & (List (Maybe (Sig key)) & (ByteString & s))))
countValidSignatures = do
  -- # Compute the number of valid signatures
  -- DIP { SWAP } ; UNPAIR @threshold @keys;
  dip swap >> unpair -- @Natural @[PublicKey]

  -- DIP
  --   {
  --     # Running count of valid signatures
  --     PUSH @valid nat 0; SWAP ;
  --     ITER
  --       {
  --         DIP { SWAP } ; SWAP ;
  --         IF_CONS
  --           {
  --             IF_SOME
  --               { SWAP ;
  --                 DIP
  --                   {
  --                     SWAP ; DIIP { DUUP } ;
  --                     # Checks signatures, fails if invalid
  --                     { DUUUP; DIP {CHECK_SIGNATURE}; SWAP; IF {DROP} {FAILWITH} };
  --                     PUSH nat 1 ; ADD @valid } }
  --               { SWAP ; DROP }
  --           }
  --           {
  --             # There were fewer signatures in the list
  --             # than keys. Not all signatures must be present, but
  --             # they should be marked as absent using the option type.
  --             FAIL
  --           } ;
  --         SWAP
  --       }
  --   } ;
  dip $ do
    push (0 :: Natural) >> swap
    iter $ do
      dip swap >> swap
      ifCons
        (ifSome
          (do
            swap
            dip $ do
              swap >> (dip $ dip $ duupX @2)
              -- duupX @3 >> dip checkSignature >> swap >> if_ drop (failCustom #invalidSignature)
              duupX @3 >> dip (checkKeySignature @key) >> swap >> if_ drop failWith
              push (1 :: Natural) >> add
          )
          (swap >> drop)
        )
        -- (failCustom #fewerSignaturesThanKeys)
        failWith
      swap


-- | Assert that the threshold is less than or equal to the
-- number of valid signatures.
assertQuorumPresent :: (Natural & (Natural & s)) :-> s
assertQuorumPresent = do
  let quorumNotPresent = [mt|Quorum not present|]
  -- # Assert that the threshold is less than or equal to the
  -- # number of valid signatures.
  -- ASSERT_CMPLE ;
  -- getField #threshold >>
  assertLe quorumNotPresent

-- | Assert no unchecked signature remains
assertAllSignaturesChecked :: forall key b c. IsKey key => ([Maybe (Sig key)] & (b & c)) :-> c
assertAllSignaturesChecked = do
  -- # Assert no unchecked signature remains
  -- IF_CONS {FAIL} {} ;
  -- DROP ;
  -- ifCons (failCustom #uncheckedSignaturesRemain) nop
  ifCons failWith nop
  drop


-- | Increment counter and place in storage
incrementAndStoreCounter :: (a & ((Natural, b) & s)) :-> (a & ((Natural, b) & s))
incrementAndStoreCounter = do
  -- # Increment counter and place in storage
  -- DIP { UNPAIR ; PUSH nat 1 ; ADD @new_counter ; PAIR} ;
  dip $ do
    unpair
    push (1 :: Natural)
    add
    pair

multisigSetup ::
     forall key a p. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
  => Proxy p
  -> (((Natural, GenericMultisigAction key a), [Maybe (Sig key)]) & (( Natural
                                                                     , ( Natural
                                                                       , [Public key])) & '[]))
  :-> (GenericMultisigAction key a & (( Natural, ( Natural, [Public key])) & '[]))
multisigSetup p = do
  assertNoTokensSent
  preparePayload p
  checkCountersMatch
  countValidSignatures @key
  assertQuorumPresent
  assertAllSignaturesChecked @key
  incrementAndStoreCounter



-- | This is an extension of the generic multisig contract:
-- - It accepts an additional parameter: @a@
-- - It stores an additional parameter: @b@
-- - It accepts a `Lambda` from @(a, b)@ to a list of `Operation`s:
--   this is a static method of extending the contract
genericMultisigContractMain :: forall a b key p.
     (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
  => Proxy p
  -> (b & (a & '[])) :-> (List Operation & '[])
  -> '[ MainParams key a, (b, Storage key)] :-> '[ ([Operation], (b, Storage key))]
genericMultisigContractMain p runParam = do
  -- { # Main entry point
  dip unpair
  swap
  -- b on top of stack

  dip $ do
    multisigSetup @key p
    -- _
    -- assertNoTokensSent
    -- preparePayload
    -- checkCountersMatch
    -- countValidSignatures @key
    -- assertQuorumPresent
    -- assertAllSignaturesChecked @key
    -- incrementAndStoreCounter
  swap >> dip swap

  -- # We have now handled the signature verification part,
  -- # produce the operation requested by the signers.
  -- IF_LEFT
  --   { # Get operation
  --     UNIT ; EXEC
  --   }
  --   {
  --     # Change set of signatures
  --     DIP { CAR } ; SWAP ; PAIR ; NIL operation
  --   };
  -- PAIR }
  caseT @(GenericMultisigAction key a)
    ( #cOperation /-> (swap >> dip (swap >> dup >> dip runParam >> swap) >> swap >> dip swap)
    , #cChangeKeys /-> (dip car >> swap >> pair >> swap >> nil)
    )
  dip pair
  pair

-- | Given a method to run the parameter type, create a
-- multisig version of the method.
genericMultisigContract ::
     forall a b key p. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
  => Proxy p
  -> (b & (a & '[])) :-> ([Operation] & '[]) -> Contract (Parameter key a) (b, Storage key)
genericMultisigContract p runParam = do
  unpair
  caseT @(Parameter key a)
    -- { # Default entry point: do nothing
    --   # This entry point can be used to send tokens to this contract
    --   DROP ; NIL operation ; PAIR }
    ( #cDefault /->
      genericMultisigContractDefault
      -- do
      -- nil @Operation
      -- pair
    , #cMainParameter /->
      genericMultisigContractMain p runParam
    )


---------------------------------------------
  --




-- | This is an extension of the generic multisig contract:
-- - It accepts an additional parameter: @a@
-- - It stores an additional parameter: @b@
-- - It accepts a `Lambda` from @(a, b)@ to a list of `Operation`s:
--   this is a static method of extending the contract
genericMultisigContractSimpleStorageMain :: forall a key p. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
  => Proxy p
  -> (a & '[]) :-> ([Operation] & '[])
  -> '[ MainParams key a, Storage key] :-> '[ ([Operation], Storage key)]
genericMultisigContractSimpleStorageMain p runParam = do
  -- { # Main entry point
  multisigSetup @key p

  -- # We have now handled the signature verification part,
  -- # produce the operation requested by the signers.
  -- IF_LEFT
  --   { # Get operation
  --     UNIT ; EXEC
  --   }
  --   {
  --     # Change set of signatures
  --     DIP { CAR } ; SWAP ; PAIR ; NIL operation
  --   };
  -- PAIR }
  caseT @(GenericMultisigAction key a)
    ( #cOperation /-> (swap >> dip runParam >> swap)
    , #cChangeKeys /-> (dip car >> swap >> pair >> nil)
    )
  pair

-- | Given a method to run the parameter type, create a
-- multisig version of the method.
genericMultisigContractSimpleStorage ::
     forall a key p. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
  => Proxy p
  -> (a & '[]) :-> ([Operation] & '[]) -> Contract (Parameter key a) (Storage key)
genericMultisigContractSimpleStorage p runParam = do
  unpair
  caseT @(Parameter key a)
    -- { # Default entry point: do nothing
    --   # This entry point can be used to send tokens to this contract
    --   DROP ; NIL operation ; PAIR }
    ( #cDefault /->
      genericMultisigContractDefault
    , #cMainParameter /->
      genericMultisigContractSimpleStorageMain p runParam
    )

-- | Generic multisig contract with pairs of keys representing "individual" signers
generigMultisigContract223 ::
  Contract
    (Parameter (PublicKey, PublicKey) (Lambda () [Operation]))
    (Storage (PublicKey, PublicKey))
generigMultisigContract223 =
  genericMultisigContractSimpleStorage (Proxy @(Parameter (PublicKey, PublicKey) (Lambda () [Operation]))) $ do
    unit
    exec


-- | A contract with an explicit top-level `BigMap`
type BigMapContract k v a b = Contract a (BigMap k v, b)

-- | The main entrypoint for the `genericMultisigContract`
wrappedMultisigContractMain ::
     forall k v a b key p. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
  => Proxy p
  -> '[ MainParams key a, (BigMap k v, ((BigMapContract k v a b, b), Storage key))] :-> '[ ( [Operation]
                                                                                   , ( BigMap k v
                                                                                     , ( ( BigMapContract k v a b
                                                                                         , b)
                                                                                       , Storage key)))]
wrappedMultisigContractMain p = do
  -- { # Main entry point
  -- b on top of stack
  dip $ do
    unpair
    dip unpair
    pair
  swap

  dip $ do
    assertNoTokensSent
    -- _ p
    preparePayload @key @a @([Maybe (Sig key)]) @(Natural, (Natural, [Public key])) p
    -- preparePayload :: forall key a b c s p. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter p)
    -- -> (((Natural, GenericMultisigAction key a), b) & (c & s)) :-> (c & (Natural & (ByteString & (b & (GenericMultisigAction key a & (c & s))))))

    checkCountersMatch
    countValidSignatures @key
    assertQuorumPresent
    assertAllSignaturesChecked @key
    incrementAndStoreCounter

  swap >> dip swap

  -- # We have now handled the signature verification part,
  -- # produce the operation requested by the signers.
  -- IF_LEFT
  --   { # Get operation
  --     UNIT ; EXEC
  --   }
  --   {
  --     # Change set of signatures
  --     DIP { CAR } ; SWAP ; PAIR ; NIL operation
  --   };
  -- PAIR }

  -- removed caseT since it required (IsoValue a)
  -- caseT @(GenericMultisigAction a)
  coerce_ @(GenericMultisigAction key a) @(Either a (ChangeKeyParams key))
  ifLeft
    (do
      dip $ do
        swap
        unpair
        dip $ do
          unpair
          swap
          dip dup
        pair
      pair
      exec
      unpair
      dip $ do
        unpair
        dip $ do
          swap
          pair
        pair
    )
    (dip car >> swap >> pair >> swap >> nil
    )

  dip $ do
    unpair
    dip pair
    pair
  pair

-- | The `WrappedMultisig` Contract
wrappedMultisigContract ::
     forall k v a b key. (IsKey key, KnownValue a, NoOperation a, NoBigMap a, NiceParameter a)
  => Contract (Parameter key a) (BigMap k v, ((BigMapContract k v a b, b), Storage key))
wrappedMultisigContract = do
  unpair
  coerce_ @(Parameter key a) @(Either () (MainParams key a)) -- Otherwise, use `MainParameter`
  ifLeft
    (drop >> genericMultisigContractDefault)
    (wrappedMultisigContractMain (Proxy @(Parameter key a)))

-- | `wrappedMultisigContract` with proxy parameters to ease specialization
wrappedMultisigContractProxy ::
     forall key proxyK proxyV proxyA proxyB k v a b.
     (IsKey key, NoOperation a, NoBigMap a, NiceParameter a)
  => proxyK k
  -> proxyV v
  -> proxyA a
  -> proxyB b
  -> Contract (Parameter key a) (BigMap k v, ((BigMapContract k v a b, b), Storage key))
wrappedMultisigContractProxy _ _ _ _ = wrappedMultisigContract

-- parameter (or (unit %default)
--               (pair %main
--                  (pair :payload
--                     (nat %counter) # counter, used to prevent replay attacks
--                     (or :action    # payload to sign, represents the requested action
--                        (lambda %operation unit (list operation))
--                        (pair %change_keys          # change the keys controlling the multisig
--                           (nat %threshold)         # new threshold
--                           (list %keys key))))     # new list of keys
--                  (list %sigs (option signature))));    # signatures
--
-- storage (pair (nat %stored_counter) (pair (nat %threshold) (list %keys key))) ;
--
-- code
--   {
--     UNPAIR ;
--     IF_LEFT
--       { # Default entry point: do nothing
--         # This entry point can be used to send tokens to this contract
--         DROP ; NIL operation ; PAIR }
--       { # Main entry point
--         # Assert no token was sent:
--         # to send tokens, the default entry point should be used
--         PUSH mutez 0 ; AMOUNT ; ASSERT_CMPEQ ;
--         SWAP ; DUP ; DIP { SWAP } ;
--         DIP
--           {
--             UNPAIR ;
--             # pair the payload with the current contract address, to ensure signatures
--             # can't be replayed accross different contracts if a key is reused.
--             DUP ; SELF ; ADDRESS ; PAIR ;
--             PACK ; # form the binary payload that we expect to be signed
--             DIP { UNPAIR @counter ; DIP { SWAP } } ; SWAP
--           } ;
--
--         # Check that the counters match
--         UNPAIR @stored_counter; DIP { SWAP };
--         ASSERT_CMPEQ ;
--
--         # Compute the number of valid signatures
--         DIP { SWAP } ; UNPAIR @threshold @keys;
--         DIP
--           {
--             # Running count of valid signatures
--             PUSH @valid nat 0; SWAP ;
--             ITER
--               {
--                 DIP { SWAP } ; SWAP ;
--                 IF_CONS
--                   {
--                     IF_SOME
--                       { SWAP ;
--                         DIP
--                           {
--                             SWAP ; DIIP { DUUP } ;
--                             # Checks signatures, fails if invalid
--                             { DUUUP; DIP {CHECK_SIGNATURE}; SWAP; IF {DROP} {FAILWITH} };
--                             PUSH nat 1 ; ADD @valid } }
--                       { SWAP ; DROP }
--                   }
--                   {
--                     # There were fewer signatures in the list
--                     # than keys. Not all signatures must be present, but
--                     # they should be marked as absent using the option type.
--                     FAIL
--                   } ;
--                 SWAP
--               }
--           } ;
--         # Assert that the threshold is less than or equal to the
--         # number of valid signatures.
--         ASSERT_CMPLE ;
--         # Assert no unchecked signature remains
--         IF_CONS {FAIL} {} ;
--         DROP ;
--
--         # Increment counter and place in storage
--         DIP { UNPAIR ; PUSH nat 1 ; ADD @new_counter ; PAIR} ;
--
--         # We have now handled the signature verification part,
--         # produce the operation requested by the signers.
--         IF_LEFT
--           { # Get operation
--             UNIT ; EXEC
--           }
--           {
--             # Change set of signatures
--             DIP { CAR } ; SWAP ; PAIR ; NIL operation
--           };
--         PAIR }
--   }

