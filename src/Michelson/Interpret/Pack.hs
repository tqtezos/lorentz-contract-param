-- | Module, carrying logic of @PACK@ instruction.
--
-- This is nearly symmetric to adjacent Unpack.hs module.
module Michelson.Interpret.Pack
  ( packValue
  , packValue'
  ) where

import Prelude hiding (EQ, GT, LT)

import Control.Exception (assert)
import qualified Data.Binary.Put as Bi
import qualified Data.Bits as Bits
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Singletons (SingI(..))

import Michelson.Text
import Michelson.Typed
import Tezos.Address (Address(..))
import Tezos.Core (Mutez(..), timestampToSeconds)
import Tezos.Crypto (KeyHash(..), PublicKey(unPublicKey), Signature(..))

-- | Serialize a value given to @PACK@ instruction.
packValue :: (SingI t, HasNoOp t, HasNoBigMap t) => Value t -> LByteString
packValue x = "\x05" <> encodeValue x

-- | Same as 'packValue', for strict bytestring.
packValue' :: (SingI t, HasNoOp t, HasNoBigMap t) => Value t -> ByteString
packValue' = LBS.toStrict . packValue

encodeValue :: forall t. (SingI t, HasNoOp t, HasNoBigMap t) => Value t -> LByteString
encodeValue val = case (val, sing @t) of
  (VC cval, _) -> encodeCValue cval
  (VKey s, _) ->
    encodeBytes "\x00" "" $
      LBS.fromStrict . ByteArray.convert $ unPublicKey s
  (VUnit, _) -> "\x03\x0b"
  (VSignature x, _) ->
    encodeBytes "" "" $
      LBS.fromStrict . ByteArray.convert $ unSignature x
  (VOption (Just x), STOption _) -> "\x05\x09" <> encodeValue x
  (VOption Nothing, _) -> "\x03\x06"
  (VList xs, STList _) -> encodeList encodeValue xs
  (VSet xs, _) -> encodeList encodeCValue (toList xs)
  (VContract addr, _) -> encodeAddress addr
  (VPair (v1, v2), STPair l _) ->
    case (checkOpPresence l, checkBigMapPresence l) of
      (OpAbsent, BigMapAbsent) -> "\x07\x07" <> encodeValue v1 <> encodeValue v2
  (VOr (Left v), STOr l _) ->
    case (checkOpPresence l, checkBigMapPresence l) of
      (OpAbsent, BigMapAbsent) -> "\x05\x05" <> encodeValue v
  (VOr (Right v), STOr l _) ->
    case (checkOpPresence l, checkBigMapPresence l) of
      (OpAbsent, BigMapAbsent) -> "\x05\x08" <> encodeValue v
  (VLam lam, _) -> encodeInstrs lam
  (VMap m, STMap _ _) -> encodeMap m

encodeCValue :: CValue t -> LByteString
encodeCValue = \case
  CvInt x        -> encodeNumeric x
  CvNat x        -> encodeNumeric x
  CvString text  -> encodeString text
  CvBytes bytes  -> encodeBytes "" "" (LBS.fromStrict bytes)
  CvMutez x      -> encodeNumeric (unMutez x)
  CvBool True    -> "\x03\x0a"
  CvBool False   -> "\x03\x03"
  CvKeyHash s    -> encodeBytes "\x00" "" (LBS.fromStrict $ unKeyHash s)
  CvTimestamp x  -> encodeNumeric (timestampToSeconds @Integer x)
  CvAddress addr -> encodeAddress addr

encodeLength :: Int -> LByteString
encodeLength = Bi.runPut . Bi.putWord32be . fromIntegral

-- | Lift encoded list content to an entire encoded list.
encodeAsList :: LByteString -> LByteString
encodeAsList bs = encodeLength (length bs) <> bs

-- | Encode a list-like structure.
encodeList :: (a -> LByteString) -> [a] -> LByteString
encodeList encodeElem l = "\x02" <> encodeAsList (LBS.concat $ map encodeElem l)

-- | Encode a text.
encodeString :: MText -> LByteString
encodeString text = "\x01" <> encodeAsList (encodeUtf8 $ unMText text)

-- | Encode some raw data.
encodeBytes :: ByteString -> ByteString -> LByteString -> LByteString
encodeBytes (LBS.fromStrict -> prefix) (LBS.fromStrict -> suffix) bs =
  "\x0a" <> encodeAsList (prefix <> bs <> suffix)

-- | Encode some map.
encodeMap :: (SingI v, HasNoOp v, HasNoBigMap v) => Map (CValue k) (Value v) -> LByteString
encodeMap m =
  encodeList (\(k, v) -> "\x07\x04" <> encodeCValue k <> encodeValue v) (Map.toList m)

encodeAddress :: Address -> LByteString
encodeAddress = \case
  KeyAddress keyHash -> encodeBytes "\x00\x00" "" (LBS.fromStrict $ unKeyHash keyHash)
  ContractAddress address -> encodeBytes "\x01" "\x00" (LBS.fromStrict address)

-- | Encode contents of a given number.
encodeIntPayload :: Integer -> LByteString
encodeIntPayload = LBS.pack . toList . doEncode True
  where
    {- Numbers are represented as follows:

    byte 0:         1              _         ______   ||  lowest digits
            has continuation  is negative   payload   ||
                                                      ||
    byte 1:         1                       _______   ||
    ...             1                       _______   ||
    byte n:         0                       _______   ||
            has continuation                payload   \/  highest digits
    -}
    doEncode :: Bool -> Integer -> NonEmpty Word8
    doEncode isFirst a
      | a >= byteWeight =
          let (hi, lo) = a `divMod` byteWeight
              byte = Bits.setBit (fromIntegral @_ @Word8 lo) 7
          in byte :| toList (doEncode False hi)
      | a >= 0 =
          one (fromIntegral @_ @Word8 a)
      | otherwise = assert isFirst $
          let h :| t = doEncode True (-a)
          in Bits.setBit h 6 :| t
      where
        byteWeight = if isFirst then 64 else 128

-- | Encode an int-like value.
encodeNumeric :: Integral i => i -> LByteString
encodeNumeric i = "\x00" <> encodeIntPayload (fromIntegral i)

-- | Encode a code block.
encodeInstrs :: Instr inp out -> LByteString
encodeInstrs = encodeList id . one . encodeInstr

-- | Encode an instruction.
encodeInstr :: forall inp out. Instr inp out -> LByteString
encodeInstr = \case
  Seq a b ->
    encodeInstr a <> encodeInstr b
  Nop ->
    mempty
  Nested i ->
    encodeInstrs i
  Ext _ ->
    ""
  DROP ->
    "\x03\x20"
  DUP ->
    "\x03\x21"
  SWAP ->
    "\x03\x4c"
  PUSH (a :: Value t) ->
    "\x07\x43" <> encodeT' @t <> encodeValue a
  SOME ->
    "\x03\x46"
  NONE | _ :: Proxy ('TOption t ': s) <- Proxy @out ->
    "\x05\x3e" <> encodeT' @t
  UNIT ->
    "\x03\x4f"
  IF_NONE a b ->
    "\x07\x2f" <> encodeInstrs a <> encodeInstrs b
  PAIR ->
    "\x03\x42"
  CAR ->
    "\x03\x16"
  CDR ->
    "\x03\x17"
  LEFT | _ :: Proxy ('TOr l r ': s) <- Proxy @out ->
    "\x05\x33" <> encodeT' @r
  RIGHT | _ :: Proxy ('TOr l r ': s) <- Proxy @out ->
    "\x05\x44" <> encodeT' @l
  IF_LEFT a b ->
    "\x07\x2e" <> encodeInstrs a <> encodeInstrs b
  NIL | _ :: Proxy ('TList t ': s) <- Proxy @out ->
    "\x05\x3d" <> encodeT' @t
  CONS ->
    "\x03\x1b"
  IF_CONS a b ->
    "\x07\x2d" <> encodeInstrs a <> encodeInstrs b
  SIZE ->
    "\x03\x45"
  EMPTY_SET | _ :: Proxy ('TSet t ': s) <- Proxy @out ->
    "\x05\x24" <> encodeT' @('Tc t)
  EMPTY_MAP | _ :: Proxy ('TMap k v ': s) <- Proxy @out ->
    "\x07\x23" <> encodeT' @('Tc k) <> encodeT' @v
  MAP a ->
    "\x05\x38" <> encodeInstrs a
  ITER a ->
    "\x05\x52" <> encodeInstrs a
  MEM ->
    "\x03\x39"
  GET ->
    "\x03\x29"
  UPDATE ->
    "\x03\x50"
  IF a b ->
    "\x07\x2c" <> encodeInstrs a <> encodeInstrs b
  LOOP a ->
    "\x05\x34" <> encodeInstrs a
  LOOP_LEFT a ->
    "\x05\x53" <> encodeInstrs a
  LAMBDA (v :: Value ('TLambda i o)) ->
    "\x09\x31" <>
    encodeAsList (encodeT' @i <> encodeT' @o <> encodeValue v) <>
    encodeLength 0  -- @martoon: dunno where does it come from
  EXEC ->
    "\x03\x26"
  DIP a ->
    "\x05\x1f" <> encodeInstrs a
  FAILWITH ->
    "\x03\x27"
  CAST | _ :: Proxy (t ': s) <- Proxy @out ->
    "\x05\x57" <> encodeT' @t
  RENAME ->
    "\x03\x58"
  PACK ->
    "\x03\x0c"
  UNPACK | _ :: Proxy ('TOption t ': s) <- Proxy @out ->
    "\x05\x0d" <> encodeT' @t
  CONCAT ->
    "\x03\x1a"
  CONCAT' ->
    "\x03\x1a"
  SLICE ->
    "\x03\x6f"
  ISNAT ->
    "\x03\x56"
  ADD ->
    "\x03\x12"
  SUB ->
    "\x03\x4b"
  MUL ->
    "\x03\x3a"
  EDIV ->
    "\x03\x22"
  ABS ->
    "\x03\x11"
  NEG ->
    "\x03\x3b"
  LSL ->
    "\x03\x35"
  LSR ->
    "\x03\x36"
  OR ->
    "\x03\x41"
  AND ->
    "\x03\x14"
  XOR ->
    "\x03\x51"
  NOT ->
    "\x03\x3f"
  COMPARE ->
    "\x03\x19"
  EQ ->
    "\x03\x25"
  NEQ ->
    "\x03\x3c"
  LT ->
    "\x03\x37"
  GT ->
    "\x03\x2a"
  LE ->
    "\x03\x32"
  GE ->
    "\x03\x28"
  INT ->
    "\x03\x30"
  SELF ->
    error "SELF should not appear in lambda"
  CONTRACT _ | _ :: Proxy ('TOption ('TContract t) ': s) <- Proxy @out ->
    "\x05\x55" <> encodeT' @t
  TRANSFER_TOKENS ->
    "\x03\x4d"
  SET_DELEGATE ->
    "\x03\x4e"
  CREATE_ACCOUNT ->
    "\x03\x1c"
  CREATE_CONTRACT (instr :: Instr '[ 'TPair p g ] '[ 'TPair ('TList 'TOperation) g ]) ->
    let contents =
          [ "\x05\x00" <> encodeT' @p
          , "\x05\x01" <> encodeT' @g
          , "\x05\x02" <> encodeInstrs instr
          ]
    -- TODO [TM-96] These ^ should be encoded in the same order in which
    -- they appear in the original code
    in "\x05\x1d" <> encodeList id contents
  IMPLICIT_ACCOUNT ->
   "\x03\x1e"
  NOW ->
   "\x03\x40"
  AMOUNT ->
   "\x03\x13"
  BALANCE ->
   "\x03\x15"
  CHECK_SIGNATURE ->
   "\x03\x18"
  SHA256 ->
   "\x03\x0f"
  SHA512 ->
   "\x03\x10"
  BLAKE2B ->
   "\x03\x0e"
  HASH_KEY ->
   "\x03\x2b"
  STEPS_TO_QUOTA ->
   "\x03\x4a"
  SOURCE ->
   "\x03\x47"
  SENDER ->
   "\x03\x48"
  ADDRESS ->
   "\x03\x54"

encodeT :: T -> LByteString
encodeT = \case
  Tc ct -> encodeCT ct
  TKey  -> "\x03\x5c"
  TUnit -> "\x03\x6c"
  TSignature -> "\x03\x67"
  TOption t -> "\x05\x63" <> encodeT t
  TList t -> "\x05\x5f" <> encodeT t
  TSet t -> "\x05\x66" <> encodeCT t
  TOperation -> "\x03\x6d"
  TContract t -> "\x05\x5a" <> encodeT t
  TPair a b -> "\x07\x65" <> encodeT a <> encodeT b
  TOr a b -> "\x07\x64" <> encodeT a <> encodeT b
  TLambda a r -> "\x07\x5e" <> encodeT a <> encodeT r
  TMap k v -> "\x07\x60" <> encodeCT k <> encodeT v
  TBigMap k v -> "\x07\x61" <> encodeCT k <> encodeT v

encodeT' :: forall (t :: T). SingI t => LByteString
encodeT' = encodeT (fromSingT $ sing @t)

encodeCT :: CT -> LByteString
encodeCT = ("\x03" <>) . \case
  CInt -> "\x5b"
  CNat -> "\x62"
  CString -> "\x68"
  CBytes -> "\x69"
  CMutez -> "\x6a"
  CBool -> "\x59"
  CKeyHash -> "\x5d"
  CTimestamp -> "\x6b"
  CAddress -> "\x6e"
