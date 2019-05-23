{- | Module, carrying logic of @UNPACK@ instruction.

This is nearly symmetric to adjacent Pack.hs module.

When implementing this the following sources were used:

* https://pastebin.com/8gfXaRvp

* https://gitlab.com/tezos/tezos/blob/master/src/proto_alpha/lib_protocol/script_ir_translator.ml#L2501

* https://github.com/tezbridge/tezbridge-crypto/blob/master/src/PsddFKi3/codec.js#L513

-}
module Michelson.Interpret.Unpack
  ( UnpackError (..)
  , unpackValue
  , unpackValue'
  , UnpackEnv (..)
  ) where

import Prelude hiding (EQ, Ordering(..), get)

import Control.Monad.Except (throwError)
import Data.Binary (Get)
import qualified Data.Binary.Get as Get
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint (Dict(..))
import Data.Default (def)
import qualified Data.Kind as Kind
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Singletons (SingI(..))
import Data.Typeable ((:~:)(..))
import Fmt (Buildable, build, fmt, hexF, (+|), (+||), (|+), (||+))
import Text.Hex (encodeHex)

import Michelson.TypeCheck
  (HST(..), SomeHST(..), SomeInstr(..), SomeInstrOut(..), TCError(..), TcOriginatedContracts,
  TypeCheckEnv(..))
import Michelson.TypeCheck.Helpers (ensureDistinctAsc, eqHST1)
import Michelson.TypeCheck.Instr (typeCheckList)
import Michelson.Typed (Sing(..))
import qualified Michelson.Typed as T
import Michelson.Typed.Scope
  (BigMapPresence(..), HasNoBigMap, HasNoOp, OpPresence(..), bigMapAbsense, checkBigMapPresence,
  checkOpPresence, opAbsense)
import Michelson.Untyped
import Tezos.Address (Address(..))
import Tezos.Core (mkMutez, timestampFromSeconds)
import Tezos.Crypto (KeyHash(..), mkPublicKey, mkSignature)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Any decoding error.
newtype UnpackError = UnpackError { unUnpackError :: Text }
  deriving (Show, Eq)

instance Buildable UnpackError where
  build (UnpackError msg) = build msg

data UnpackEnv = UnpackEnv
  { ueContracts :: TcOriginatedContracts
  }

-- | Alias for label attaching.
(?) :: Get a -> String -> Get a
(?) = flip Get.label
infix 0 ?

-- | Get a bytestring of the given length leaving no references to the
-- original data in serialized form.
getByteStringCopy :: Int -> Get ByteString
getByteStringCopy = fmap BS.copy . Get.getByteString

-- | Read a byte and match it against given value.
expectTag :: String -> Word8 -> Get ()
expectTag desc t =
  Get.label desc $ do
    t' <- Get.getWord8
    unless (t == t') $
      fail . fmt $ "Unexpected tag value (expected 0x" +| hexF t |+
                   ", but got 0x" +| hexF t' |+ ")"

-- | Fail with "unknown tag" error.
unknownTag :: Text -> Word8 -> Get a
unknownTag desc tag =
  fail . fmt $ "Unknown " <> build desc <> " tag: 0x" <> hexF tag

-- | Read a byte describing the primitive going further and match it against
-- expected tag in the given conditions.
--
-- Aside of context description, you have to specify number of arguments which
-- given instruction accepts when written in Michelson. For instance, @PUSH@
-- accepts two arguments - type and value.
expectDescTag :: HasCallStack => String -> Word16 -> Get ()
expectDescTag desc argsNum =
  Get.label desc $ do
    tag <- Get.getWord8
    unless (tag == expected) $
      fail . fmt $ "Unexpected preliminary tag: 0x" <> hexF tag
  where
    expected = case argsNum of
      0 -> 0x03
      1 -> 0x05
      2 -> 0x07
      3 -> 0x08
      _ -> error "Bad arguments num"
      -- Intermediate values of tag are also used and designate that annotations
      -- are also attached to the packed data. But they are never produced by
      -- @PACK@, neither @UNPACK@ seem to expect them, so for now we pretend
      -- that annotations do not exist.

ensureEnd :: Get ()
ensureEnd =
  unlessM Get.isEmpty $ do
    remainder <- Get.getRemainingLazyByteString
    fail $ "Expected end of entry, unconsumed bytes \
           \(" +| length remainder |+ "): "
           +|| encodeHex (LBS.toStrict remainder) ||+ ""

-- | Like 'many', but doesn't backtrack if next entry failed to parse
-- yet there are some bytes to consume ahead.
--
-- This function exists primarily for better error messages.
manyForced :: Get a -> Get [a]
manyForced decode = do
  emp <- Get.isEmpty
  if emp
    then return []
    else (:) <$> decode <*> manyForced decode

----------------------------------------------------------------------------
-- Michelson serialisation
----------------------------------------------------------------------------

{- Implementation notes:

* We need to know which exact type we unpack to.
For instance, serialized signatures are indistinguishable from
plain serialized bytes, so if we want to return "Value" (typed or untyped),
we need to know currently expected type. The reference implementation does
the same.

* It occured to be easier to decode to typed values and untyped instructions.
When decoding lambda, we type check given instruction, and when decoding
@PUSH@ call we untype decoded value.
One may say that this gives unreasonable performance overhead, but with the
current definition of "Value" types (typed and untyped) we cannot avoid it
anyway, because when deserializing bytearray-like data (keys, signatures, ...),
we have to convert raw bytes to human-readable 'Text' and later parse them
to bytes back at type check stage.
We console ourselves that lambdas are rarely packed.

-}

-- | Deserialize bytes into the given value.
-- Suitable for @UNPACK@ operation only.
unpackValue
  :: (SingI t, HasNoOp t, HasNoBigMap t)
  => UnpackEnv -> LByteString -> Either UnpackError (T.Value t)
unpackValue env bs =
  case Get.runGetOrFail (unpackDecoder env) bs of
    Left (_remainder, _offset, err) -> Left . UnpackError $ toText err
    Right (_remainder, _offset, res) -> Right res

-- | Like 'unpackValue', for strict byte array.
unpackValue'
  :: (SingI t, HasNoOp t, HasNoBigMap t)
  => UnpackEnv -> ByteString -> Either UnpackError (T.Value t)
unpackValue' env = unpackValue env . LBS.fromStrict

-- | Overall value decoder we use in @UNPACK@.
unpackDecoder
  :: (SingI t, HasNoOp t, HasNoBigMap t)
  => UnpackEnv -> Get (T.Value t)
unpackDecoder env =
  expectTag "Packed data start" 0x05 *> decodeValue env <* ensureEnd

decodeValue
  :: forall t.
     (SingI t, HasNoOp t, HasNoBigMap t)
  => UnpackEnv -> Get (T.Value t)
decodeValue env = Get.label "Value" $
  case sing @t of
    STc _ ->
      T.VC <$> decodeCValue
    STKey ->
      decodeAsBytes $ do
        expectTag "Key pad" 0x00
        bs <- getByteStringCopy 32
        case mkPublicKey bs of
          Left err -> fail $ "Wrong public key format: " <> toString err
          Right pk -> pure (T.VKey pk)
    STUnit -> do
      expectDescTag "Unit" 0
      expectTag "Unit" 0x0B
      return T.VUnit
    STSignature -> do
      decodeAsBytes $ do
        bs <- getByteStringCopy 64
        case mkSignature bs of
          Left err -> fail $ "Wrong signature format: " <> toString err
          Right s -> pure (T.VSignature s)
    STOption _ -> do
      Get.getByteString 2 >>= \case
        "\x03\x06" -> pure (T.VOption Nothing)
        "\x05\x09" -> T.VOption . Just <$> decodeValue env
        other -> fail $ "Unknown option tag: " <> show other
    STList _ -> do
      decodeAsList $ T.VList <$> manyForced (decodeValue env)
    STSet _ -> do
      decodeAsList $ do
        vals <- manyForced decodeCValue
        either (fail . toString) pure $
          T.VSet . Set.fromDistinctAscList <$> ensureDistinctAsc id vals
    STContract _ ->
      T.VContract <$> decodeAddress
    STPair lt _ ->
      case (checkOpPresence lt, checkBigMapPresence lt) of
        (OpAbsent, BigMapAbsent) -> do
          expectDescTag "Pair" 2
          expectTag "Pair" 0x07
          T.VPair ... (,) <$> decodeValue env <*> decodeValue env
    STOr lt _ ->
      case (checkOpPresence lt, checkBigMapPresence lt) of
        (OpAbsent, BigMapAbsent) -> do
          expectDescTag "Or" 1
          Get.getWord8 >>= \case
            0x05 -> T.VOr . Left <$> decodeValue env
            0x08 -> T.VOr . Right <$> decodeValue env
            other -> unknownTag "or constructor" other
    STLambda _ _ -> do
      uinstr <- decodeOps env
      T.VLam <$> decodeTypeCheckLam env uinstr
    STMap _ _ -> do
      T.VMap <$> decodeMap env

decodeCValue :: forall ct. SingI ct => Get (T.CValue ct)
decodeCValue = case sing @ct of
  -- TODO [TM-140]: The reference implementation allows to decode some
  -- of cases below both from bytes and from string; consider this.
  SCInt -> do
    expectTag "Int" 0x00
    T.CvInt <$> decodeInt
  SCNat -> do
    expectTag "Nat" 0x00
    T.CvNat <$> decodeInt
  SCString -> do
    expectTag "String" 0x01
    T.CvString <$> decodeString
  SCBytes -> do
    expectTag "Bytes" 0x0a
    T.CvBytes <$> decodeBytes
  SCMutez -> do
    expectTag "Mutez" 0x00
    mmutez <- mkMutez <$> decodeInt
    maybe (fail "Negative mutez") (pure . T.CvMutez) mmutez
  SCBool -> do
    expectDescTag "Bool" 0
    Get.getWord8 >>= \case
      0x0A -> pure (T.CvBool True)
      0x03 -> pure (T.CvBool False)
      other -> unknownTag "bool" other
  SCKeyHash ->
    decodeAsBytes $ do
      expectTag "key address pad" 0x00
      T.CvKeyHash . KeyHash <$> getByteStringCopy 20
  SCTimestamp -> do
    expectTag "Timestamp" 0x00
    T.CvTimestamp . timestampFromSeconds @Integer <$> decodeInt
  SCAddress ->
    T.CvAddress <$> decodeAddress

-- | Read length of something (list, string, ...).
decodeLength :: Get Int
decodeLength = Get.label "Length" $ do
  len <- Get.getWord32be
  -- @martoon: I'm not sure whether returning 'Int' is valid here.
  -- Strictly speaking, it may be 'Word32', but there seems to be no easy way
  -- to check the reference implementation on that.
  -- One more reason to go with just 'Int' for now is that we need to be able to
  -- deserialize byte arrays, and 'BS.ByteString' keeps length of type 'Int'
  -- inside.
  let len' = fromIntegral @_ @Int len
  unless (fromIntegral len' == len && len' >= 0) $
    fail "Length overflow"
  return len'

decodeAsListRaw :: Get a -> Get a
decodeAsListRaw getElems = do
  l <- decodeLength ? "List length"
  Get.isolate l (getElems ? "List content")

-- | Given decoder for list content, get a whole list decoder.
decodeAsList :: Get a -> Get a
decodeAsList getElems = do
  expectTag "List" 0x02
  decodeAsListRaw getElems

decodeString :: Get Text
decodeString = do
  l <- decodeLength ? "String length"
  ss <- replicateM l Get.getWord8 ? "String content"
  decodeUtf8' (BS.pack ss)
    & either (fail . show) pure
    ? "String UTF-8 decoding"

decodeAsBytesRaw :: (Int -> Get a) -> Get a
decodeAsBytesRaw decode = do
  l <- decodeLength ? "Byte array length"
  decode l ? "Byte array content"

decodeAsBytes :: Get a -> Get a
decodeAsBytes decode = do
  expectTag "Bytes" 0x0A
  decodeAsBytesRaw (const decode)

decodeBytes :: Get ByteString
decodeBytes = decodeAsBytesRaw getByteStringCopy

decodeMap
  :: (SingI k, SingI v, HasNoOp v, HasNoBigMap v)
  => UnpackEnv -> Get $ Map (T.CValue k) (T.Value v)
decodeMap env = Get.label "Map" $
  decodeAsList $ do
    es <- manyForced $ do
      expectDescTag "Elt" 2
      expectTag "Elt" 0x04
      (,) <$> decodeCValue <*> decodeValue env
    either (fail . toString) pure $
      Map.fromDistinctAscList <$> ensureDistinctAsc fst es

decodeAddress :: Get Address
decodeAddress = Get.label "Address" $
  decodeAsBytes $ (Get.getWord8 ? "Address tag") >>= \case
    0x00 -> Get.label "Plain address" $ do
      expectTag "key address pad" 0x00
      KeyAddress . KeyHash <$> getByteStringCopy 20
    0x01 -> Get.label "Contract address" $ do
      addr <- getByteStringCopy 20
      expectTag "contract address pad" 0x00
      return $ ContractAddress addr
    other -> unknownTag "address" other

-- | Read a numeric value.
decodeInt :: Num i => Get i
decodeInt = fromIntegral @Integer <$> loop 0 0 ? "Number"
  where
    loop !offset !acc = do
      byte <- Get.getWord8

      let hasCont = Bits.testBit byte 7
      let doCont shft = if hasCont then loop (shft + offset) else pure
      let addAndCont shft bytePayload =
            doCont shft $ acc + Bits.shiftL (fromIntegral bytePayload) offset

      let payload = Bits.clearBit byte 7
      if offset > 0
        then addAndCont 7 payload
        else do
          let sign = if Bits.testBit byte 6 then -1 else 1
          let upayload = Bits.clearBit payload 6
          (sign *) <$> addAndCont 6 upayload

-- | For @UNPACK@ we do not consider annotations at all.
-- If they start matter for other purposes some day, remove this function.
decodeAnn :: forall (t :: Kind.Type). Get (Annotation t)
decodeAnn = pure noAnn

-- | Type check instruction occured from a lambda.
decodeTypeCheckLam
  :: forall inp out m.
     (Typeable inp, SingI inp, SingI out, Typeable out, MonadFail m)
  => UnpackEnv
  -> [ExpandedOp]
  -> m (T.Instr '[inp] '[out])
decodeTypeCheckLam UnpackEnv{..} uinstr =
  either tcErrToFail pure . evaluatingState tcInitEnv . runExceptT $ do
    let inp = (sing @inp, T.NStar, noAnn) ::& SNil
    _ :/ instr' <- typeCheckList uinstr inp
    case instr' of
      instr ::: out' ->
        case eqHST1 @out out' of
          Right Refl ->
            pure instr
          Left err ->
                -- dummy types, we have no full information to build untyped
                -- 'T' anyway
            let tinp = Type TUnit noAnn
                tout = Type TUnit noAnn
            in throwError $
              TCFailedOnInstr (LAMBDA noAnn tinp tout uinstr) (SomeHST inp)
              "Unexpected lambda output type" def (Just err)
      AnyOutInstr instr ->
        return instr
  where
    tcErrToFail err = fail $ "Type check failed: " +| err |+ ""
    tcInitEnv =
      TypeCheckEnv
      { tcExtFrames = error "runInstrImpl(UNPACK): tcExtFrames touched"
        --- ^ This is safe because @UNPACK@ never produces Ext instructions
      , tcContractParam = error "runInstrImpl(UNPACK): tcContractParam touched"
        --- ^ Used only in @SELF@ interpretation,
        ---   but there is no way for @SELF@ to appear in packed data
      , tcContracts = ueContracts
      }

decodeInstr :: UnpackEnv -> Get ExpandedInstr
decodeInstr env = Get.label "Instruction" $ do
  pretag <- Get.getWord8 ? "Pre instr tag"
  tag <- Get.getWord8 ? "Instr tag"
  case (pretag, tag) of
    (0x03, 0x20) -> pure $ DROP
    (0x03, 0x21) -> pure $ DUP noAnn
    (0x03, 0x4C) -> pure $ SWAP
    (0x07, 0x43) -> do
      an :: VarAnn <- decodeAnn
      typ <- decodeType
      T.withSomeSingT (T.fromUType typ) $ \(st :: Sing t) ->
        case (opAbsense st, bigMapAbsense st) of
          (Nothing, _) -> fail "Operation type in PUSH"
          (_, Nothing) -> fail "BigMap type in PUSH"
          (Just Dict, Just Dict) -> do
            tval <- decodeValue @t env
            return $ PUSH an typ (T.untypeValue tval)
    (0x03, 0x46) -> SOME <$> decodeAnn <*> decodeAnn <*> decodeAnn
    (0x05, 0x3E) -> NONE <$> decodeAnn <*> decodeAnn <*> decodeAnn <*> decodeType
    (0x03, 0x4F) -> UNIT <$> decodeAnn <*> decodeAnn
    (0x07, 0x2F) -> IF_NONE <$> decodeOps env <*> decodeOps env
    (0x03, 0x42) -> PAIR <$> decodeAnn <*> decodeAnn <*> decodeAnn <*> decodeAnn
    (0x03, 0x16) -> CAR <$> decodeAnn <*> decodeAnn
    (0x03, 0x17) -> CDR <$> decodeAnn <*> decodeAnn
    (0x05, 0x33) -> LEFT <$> decodeAnn <*> decodeAnn <*> decodeAnn <*> decodeAnn
                         <*> decodeType
    (0x05, 0x44) -> RIGHT <$> decodeAnn <*> decodeAnn <*> decodeAnn <*> decodeAnn
                          <*> decodeType
    (0x07, 0x2E) -> IF_LEFT <$> decodeOps env <*> decodeOps env
    (0x05, 0x3D) -> NIL <$> decodeAnn <*> decodeAnn <*> decodeType
    (0x03, 0x1B) -> CONS <$> decodeAnn
    (0x07, 0x2D) -> IF_CONS <$> decodeOps env <*> decodeOps env
    (0x03, 0x45) -> SIZE <$> decodeAnn
    (0x05, 0x24) -> EMPTY_SET <$> decodeAnn <*> decodeAnn <*> decodeComparable
    (0x07, 0x23) -> EMPTY_MAP <$> decodeAnn <*> decodeAnn <*> decodeComparable
                              <*> decodeType
    (0x05, 0x38) -> MAP <$> decodeAnn <*> decodeOps env
    (0x05, 0x52) -> ITER <$> decodeOps env
    (0x03, 0x39) -> MEM <$> decodeAnn
    (0x03, 0x29) -> GET <$> decodeAnn
    (0x03, 0x50) -> pure UPDATE
    (0x07, 0x2C) -> IF <$> decodeOps env <*> decodeOps env
    (0x05, 0x34) -> LOOP <$> decodeOps env
    (0x05, 0x53) -> LOOP_LEFT <$> decodeOps env
    (0x09, 0x31) -> do
      res <- decodeAsListRaw $
        LAMBDA <$> decodeAnn <*> decodeType <*> decodeType <*> decodeOps env
      void decodeLength
      return res
    (0x03, 0x26) -> EXEC <$> decodeAnn
    (0x05, 0x1F) -> DIP <$> decodeOps env
    (0x03, 0x27) -> pure FAILWITH
    (0x05, 0x57) -> CAST <$> decodeAnn <*> decodeType
    (0x03, 0x58) -> RENAME <$> decodeAnn
    (0x03, 0x0C) -> PACK <$> decodeAnn
    (0x05, 0x0D) -> UNPACK <$> decodeAnn <*> decodeType
    (0x03, 0x1A) -> CONCAT <$> decodeAnn
    (0x03, 0x6F) -> SLICE <$> decodeAnn
    (0x03, 0x56) -> ISNAT <$> decodeAnn
    (0x03, 0x12) -> ADD <$> decodeAnn
    (0x03, 0x4B) -> SUB <$> decodeAnn
    (0x03, 0x3A) -> MUL <$> decodeAnn
    (0x03, 0x22) -> EDIV <$> decodeAnn
    (0x03, 0x11) -> ABS <$> decodeAnn
    (0x03, 0x3B) -> pure NEG
    (0x03, 0x35) -> LSL <$> decodeAnn
    (0x03, 0x36) -> LSR <$> decodeAnn
    (0x03, 0x41) -> OR <$> decodeAnn
    (0x03, 0x14) -> AND <$> decodeAnn
    (0x03, 0x51) -> XOR <$> decodeAnn
    (0x03, 0x3F) -> NOT <$> decodeAnn
    (0x03, 0x19) -> COMPARE <$> decodeAnn
    (0x03, 0x25) -> EQ <$> decodeAnn
    (0x03, 0x3C) -> NEQ <$> decodeAnn
    (0x03, 0x37) -> LT <$> decodeAnn
    (0x03, 0x2A) -> GT <$> decodeAnn
    (0x03, 0x32) -> LE <$> decodeAnn
    (0x03, 0x28) -> GE <$> decodeAnn
    (0x03, 0x30) -> INT <$> decodeAnn
    (0x05, 0x55) -> CONTRACT <$> decodeAnn <*> decodeType
    (0x03, 0x4D) -> TRANSFER_TOKENS <$> decodeAnn
    (0x03, 0x4E) -> SET_DELEGATE <$> decodeAnn
    (0x03, 0x1C) -> CREATE_ACCOUNT <$> decodeAnn <*> decodeAnn
    (0x05, 0x1D) ->
      decodeAsList $ do
        an1 <- decodeAnn
        an2 <- decodeAnn
        expectTag "Pre contract parameter" 0x05
        expectTag "Contract parameter" 0x00
        p <- decodeType
        expectTag "Pre contract storage" 0x05
        expectTag "Contract storage" 0x01
        s <- decodeType
        expectTag "Pre contract code" 0x05
        expectTag "Contract code" 0x02
        c <- decodeOps env
        return $ CREATE_CONTRACT an1 an2 (Contract p s c)
    (0x03, 0x1E) -> IMPLICIT_ACCOUNT <$> decodeAnn
    (0x03, 0x40) -> NOW <$> decodeAnn
    (0x03, 0x13) -> AMOUNT <$> decodeAnn
    (0x03, 0x15) -> BALANCE <$> decodeAnn
    (0x03, 0x18) -> CHECK_SIGNATURE <$> decodeAnn
    (0x03, 0x0F) -> SHA256 <$> decodeAnn
    (0x03, 0x10) -> SHA512 <$> decodeAnn
    (0x03, 0x0E) -> BLAKE2B <$> decodeAnn
    (0x03, 0x2B) -> HASH_KEY <$> decodeAnn
    (0x03, 0x4A) -> STEPS_TO_QUOTA <$> decodeAnn
    (0x03, 0x47) -> SOURCE <$> decodeAnn
    (0x03, 0x48) -> SENDER <$> decodeAnn
    (0x03, 0x54) -> ADDRESS <$> decodeAnn
    (other1, other2) -> fail $ "Unknown instruction tag: 0x" +|
                        hexF other1 |+ hexF other2 |+ ""

decodeOp :: UnpackEnv -> Get ExpandedOp
decodeOp env = Get.label "Op" $ do
  tag <- Get.lookAhead Get.getWord8
  if tag == 0x02
    then SeqEx <$> decodeOps env ? "Ops seq"
    else PrimEx <$> decodeInstr env ? "One op"

decodeOps :: UnpackEnv -> Get [ExpandedOp]
decodeOps env = decodeAsList $ manyForced (decodeOp env)

decodeComparable :: Get Comparable
decodeComparable = Get.label "Comparable primitive type" $
  Comparable <$> decodeCT <*> decodeAnn

decodeCT :: Get CT
decodeCT = Get.label "CT" $ do
  pretag <- Get.getWord8 ? "Pre simple comparable type tag"
  tag <- Get.getWord8 ? "Simple comparable type tag"
  case (pretag, tag) of
    (0x03, 0x5B) -> pure CInt
    (0x03, 0x62) -> pure CNat
    (0x03, 0x68) -> pure CString
    (0x03, 0x69) -> pure CBytes
    (0x03, 0x6A) -> pure CMutez
    (0x03, 0x59) -> pure CBool
    (0x03, 0x5D) -> pure CKeyHash
    (0x03, 0x6B) -> pure CTimestamp
    (0x03, 0x6E) -> pure CAddress
    (other1, other2) -> fail $ "Unknown primitive tag: 0x" +|
                        hexF other1 |+ hexF other2 |+ ""

decodeT :: Get T
decodeT = Get.label "T" $
  doDecode <|> (Tc <$> decodeCT)
  where
    doDecode = do
      pretag <- Get.getWord8 ? "Pre complex type tag"
      tag <- Get.getWord8 ? "Complex type tag"
      case (pretag, tag) of
        (0x03, 0x5C) -> pure TKey
        (0x03, 0x6C) -> pure TUnit
        (0x03, 0x67) -> pure TSignature
        (0x05, 0x63) -> TOption <$> decodeAnn <*> decodeType
        (0x05, 0x5F) -> TList <$> decodeType
        (0x05, 0x66) -> TSet <$> decodeComparable
        (0x03, 0x6D) -> pure TOperation
        (0x05, 0x5A) -> TContract <$> decodeType
        (0x07, 0x65) -> TPair <$> decodeAnn <*> decodeAnn <*> decodeType <*> decodeType
        (0x07, 0x64) -> TOr <$> decodeAnn <*> decodeAnn <*> decodeType <*> decodeType
        (0x07, 0x5E) -> TLambda <$> decodeType <*> decodeType
        (0x07, 0x60) -> TMap <$> decodeComparable <*> decodeType
        (0x07, 0x61) -> TBigMap <$> decodeComparable <*> decodeType
        (other1, other2) -> fail $ "Unknown primitive tag: 0x" +|
                            hexF other1 |+ hexF other2 |+ ""

decodeType :: Get Type
decodeType = Type <$> decodeT <*> decodeAnn ? "Type"
