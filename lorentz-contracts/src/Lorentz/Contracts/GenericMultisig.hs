{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

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

import Lorentz
import Lorentz.Contracts.Util ()

import Text.Read

-- {-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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
type ChangeKeyParams =
  ( Natural     -- "threshold" :!
  , [PublicKey] -- "keys"      :!
  )


-- | Either perform an `Operation` with the included contract or
-- use `ChangeKeys` to update the key list and threshold (quorum)
-- @
--  type GenericMultisigAction a = Either a ChangeKeyParams
-- @
data GenericMultisigAction a
  = Operation !a
  | ChangeKeys !ChangeKeyParams
  deriving stock Generic
  deriving stock Read
  deriving anyclass IsoValue

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
type MainParams a =
  ( ( Natural                 -- "counter" :!
    , GenericMultisigAction a  -- "action"  :!
    )
  , [Maybe Signature]       -- "sigs"    :!
  )

-- | Use `Default` to send tokens to the contract.
-- Otherwise, use `MainParameter`
data Parameter a
  = Default
  | MainParameter (MainParams a)
  deriving stock Generic
  deriving stock Read
  deriving anyclass IsoValue

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
type Storage =
  ( Natural -- "storedCounter" :!
  , ( Natural     -- "threshold" :!
    , [PublicKey] -- "keys"      :!
    )
  )


-- | Errors specific to this implementation.
data GenericMultisigError
  = InvalidSignature
  | FewerSignaturesThanKeys
  | UncheckedSignaturesRemain
  deriving stock (Eq, Generic)

deriveCustomError ''GenericMultisigError

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
preparePayload :: (KnownValue a, NoOperation a, NoBigMap a) =>
     (((Natural, GenericMultisigAction a), b) & (c & s)) :-> (c & (Natural & (ByteString & (b & (GenericMultisigAction a & (c & s))))))
preparePayload = do
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
    dup >> self >> address >> pair
    pack
    dip (unpair @Natural >> dip swap) >> swap


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
countValidSignatures ::
     ((a, [PublicKey]) & (ByteString & (List (Maybe Signature) & s))) :-> (a & (Natural & (List (Maybe Signature) & (ByteString & s))))
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
              swap >> (dipX @2) (duupX @2)
              duupX @3 >> dip checkSignature >> swap >> if_ drop (failUsing InvalidSignature)
              push (1 :: Natural) >> add
          )
          (swap >> drop)
        )
        (failUsing FewerSignaturesThanKeys)
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
assertAllSignaturesChecked :: ([a] & (b & c)) :-> c
assertAllSignaturesChecked = do
  -- # Assert no unchecked signature remains
  -- IF_CONS {FAIL} {} ;
  -- DROP ;
  ifCons (failUsing UncheckedSignaturesRemain) nop
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

-- | This is an extension of the generic multisig contract:
-- - It accepts an additional parameter: @a@
-- - It stores an additional parameter: @b@
-- - It accepts a `Lambda` from @(a, b)@ to a list of `Operation`s:
--   this is a static method of extending the contract
genericMultisigContractMain :: forall a b.
     (KnownValue a, NoOperation a, NoBigMap a, IsoValue a)
  => (b & (a & '[])) :-> (List Operation & '[])
  -> '[ MainParams a, (b, Storage)] :-> '[ ([Operation], (b, Storage))]
genericMultisigContractMain runParam = do
  -- { # Main entry point
  dip unpair
  swap
  -- b on top of stack

  dip $ do
    assertNoTokensSent
    preparePayload
    checkCountersMatch
    countValidSignatures
    assertQuorumPresent
    assertAllSignaturesChecked
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
  caseT @(GenericMultisigAction a)
    ( #cOperation /-> (swap >> dip (swap >> dup >> dip runParam >> swap) >> swap >> dip swap)
    , #cChangeKeys /-> (dip car >> swap >> pair >> swap >> nil)
    )
  dip pair
  pair


genericMultisigContract ::
     forall a b. (IsoValue a, KnownValue a, NoOperation a, NoBigMap a)
  => (b & (a & '[])) :-> ([Operation] & '[]) -> Contract (Parameter a) ( b
                                                                       , Storage)
genericMultisigContract runParam = do
  unpair
  caseT @(Parameter a)
    -- { # Default entry point: do nothing
    --   # This entry point can be used to send tokens to this contract
    --   DROP ; NIL operation ; PAIR }
    ( #cDefault /->
      genericMultisigContractDefault
      -- do
      -- nil @Operation
      -- pair
    , #cMainParameter /->
      genericMultisigContractMain runParam
    )



type BigMapContract k v a b = Contract a (BigMap k v, b)

wrappedMultisigContractMain ::
     forall k v a b. (KnownValue a, NoOperation a, NoBigMap a)
  => '[ MainParams a, (BigMap k v, ((BigMapContract k v a b, b), Storage))] :-> '[ ( [Operation]
                                                                                   , ( BigMap k v
                                                                                     , ( ( BigMapContract k v a b
                                                                                         , b)
                                                                                       , Storage)))]
wrappedMultisigContractMain = do
  -- { # Main entry point
  -- b on top of stack
  dip $ do
    unpair
    dip unpair
    pair
  swap

  dip $ do
    assertNoTokensSent
    preparePayload
    checkCountersMatch
    countValidSignatures
    assertQuorumPresent
    assertAllSignaturesChecked
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
  coerce_ @(GenericMultisigAction a) @(Either a ChangeKeyParams)
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
     forall k v a b. (KnownValue a, NoOperation a, NoBigMap a)
  => Contract (Parameter a) (BigMap k v, ((BigMapContract k v a b, b), Storage))
wrappedMultisigContract = do
  unpair
  coerce_ @(Parameter a) @(Either () (MainParams a)) -- Otherwise, use `MainParameter`
  ifLeft (drop >> genericMultisigContractDefault) wrappedMultisigContractMain

-- | `wrappedMultisigContract` with proxy parameters to ease specialization
wrappedMultisigContractProxy ::
     forall proxyK proxyV proxyA proxyB k v a b.
     (KnownValue a, NoOperation a, NoBigMap a)
  => proxyK k
  -> proxyV v
  -> proxyA a
  -> proxyB b
  -> Contract (Parameter a) (BigMap k v, ((BigMapContract k v a b, b), Storage))
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

