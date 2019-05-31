{-# LANGUAGE OverloadedLists #-}

module Test.Serialization.Michelson
  ( spec_Packing
  ) where

import Prelude hiding (Ordering(..))

import Data.Singletons (SingI(..))
import qualified Data.Text as T
import Data.Typeable ((:~:)(..), Typeable, eqT, typeRep)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Hex (decodeHex, encodeHex)

import Michelson.Interpret (runUnpack)
import Michelson.Interpret.Pack (packValue')
import Michelson.Macro (expandList)
import qualified Michelson.Parser as Parser
import Michelson.Test.Util
import Michelson.TypeCheck (HST(..), SomeInstr(..), SomeInstrOut(..), typeCheckList)
import Michelson.Typed
import Michelson.Text
import Michelson.Untyped (noAnn)
import Test.Util.Parser
import Tezos.Address (Address(..), unsafeParseAddress)
import Tezos.Core (Mutez, Timestamp, timestampFromSeconds, unsafeMkMutez)
import Tezos.Crypto (KeyHash(..), parseKeyHash, parsePublicKey, parseSignature)

spec_Packing :: Spec
spec_Packing = do
  describe "pack tests for comparable values (CValue)" $ do
    intTest
    natTest
    stringTest
    bytesTest
    mutezTest
    boolTest
    keyHashTest
    timestampTest
    addressTest

  describe "pack tests for non-comparable values" $ do
    keyTest
    unitTest
    signatureTest
    optionTest
    listTest
    setTest
    contractTest
    pairTest
    orTest
    mapTest

  describe "pack tests for instructions" $ do
    instrTest
    typesTest

  unpackNegTest

stripOptional0x :: Text -> Text
stripOptional0x h = T.stripPrefix "0x" h ?: h

-- | Dummy wrapper for what do we test - pack or unpack.
data TestMethod = TestMethod
  { _tmName :: String
  , _tmApply
      :: forall t. (Typeable t, SingI t, HasNoOp t, HasNoBigMap t)
      => Value t -> Text -> Expectation
  }

testMethods :: HasCallStack => [TestMethod]
testMethods =
  [ TestMethod "Pack" $
      \val encodedHex ->
        encodeHex (packValue' val) `shouldBe` stripOptional0x encodedHex

  , TestMethod "Unpack" $
      \val encodedHex ->
        let encoded = decodeHex (stripOptional0x encodedHex)
                      ?: error ("Invalid hex: " <> show encodedHex)
        in runUnpack mempty encoded
            `shouldBe` Right val
  ]

packSpecManual
    :: (Show x, Typeable t, SingI t, HasNoOp t, HasNoBigMap t, HasCallStack)
    => String -> (x -> Value t) -> [(x, Text)] -> Spec
packSpecManual name toVal' suites =
  forM_ @[_] testMethods $ \(TestMethod mname method) ->
    describe mname $
      describe name $ forM_ suites $ \(x, h) ->
        it (show x) $ method (toVal' x) h

packSpec
  :: forall x (t :: T).
     (Typeable t, IsoValue x, Show x, ToT x ~ t, SingI t, HasNoOp t, HasNoBigMap t
     , HasCallStack)
  => [(x, Text)]
  -> Spec
packSpec = packSpecManual typeName toVal
  where
    typeName = show $ typeRep (Proxy @(Value t))

parsePackSpec
  :: forall (inp :: T) (out :: T).
     (Each [Typeable, SingI] [inp, out], HasCallStack)
  => String
  -> [(Text, Text)]
  -> Spec
parsePackSpec name suites =
  forM_ @[_] testMethods $ \(TestMethod mname method) ->
    describe mname $
      describe name $ forM_ suites $ \(codeText, packed) ->
        it (truncateName $ toString codeText) $ do
          parsed <- Parser.codeEntry `shouldParse` codeText
          let code = expandList parsed
          let _ :/ typed = typeCheckList code initStack
                & runExceptT
                & evaluatingState initTypeCheckST
                & leftToShowPanic

          case typed of
            AnyOutInstr instr ->
              method (VLam @inp @out instr) packed

            (instr :: Instr '[inp] outs) ::: _ ->
              case eqT @'[out] @outs of
                Just Refl ->
                  method (VLam @inp @out instr) packed
                Nothing ->
                  error "Output type unexpectedly mismatched"
  where
    truncateName s
      | length s < 60 = s
      | otherwise = take 60 s <> " ..."
    initTypeCheckST = error "Type check state is not defined"
    initStack = (sing @inp, NStar, noAnn) ::& SNil

unpackNegSpec
  :: forall (t :: T).
      (SingI t, HasNoOp t, HasNoBigMap t)
  => String -> Text -> Spec
unpackNegSpec name encodedHex =
  it name $
    let encoded = decodeHex (stripOptional0x encodedHex)
                  ?: error ("Invalid hex: " <> show encodedHex)
    in runUnpack @t mempty encoded
        `shouldSatisfy` isLeft

-- | Helper for defining tests cases for 'packSpec'.
-- Read it as "is packed as".
(~:) :: a -> b -> (a, b)
(~:) = (,)
infix 0 ~:

intTest :: Spec
intTest =
  packSpec
    @Integer
    [ (-64, "0500c001")
    , (-63, "05007f")
    , (-2,  "050042")
    , (-1,  "050041")
    , (0,   "050000")
    , (1,   "050001")
    , (2,   "050002")
    , (63,  "05003f")
    , (64,  "05008001")
    , (65,  "05008101")
    , (-65, "0500c101")
    , (127, "0500bf01")
    , (128, "05008002")
    , (129, "05008102")
    , (-127, "0500ff01")
    , (191, "0500bf02")
    , (192, "05008003")
    , (193, "05008103")
    , (2028, "0500ac1f")
    , (5000, "0500884e")
    , (10000, "0500909c01")
    , (20000, "0500a0b802")
    , (-5000, "0500c84e")
    , (-10000, "0500d09c01")
    , (-20000, "0500e0b802")
    ]

natTest :: Spec
natTest =
  packSpec
    @Natural
    [ (0, "050000")
    , (1, "050001")
    , (63, "05003f")
    , (64, "05008001")
    , (65, "05008101")
    , (127, "0500bf01")
    , (128, "05008002")
    , (129, "05008102")
    , (191, "0500bf02")
    , (192, "05008003")
    , (193, "05008103")
    ]

stringTest :: Spec
stringTest =
  packSpec
    @MText
    [ [mt|Hello World!|] ~: "05010000000c48656c6c6f20576f726c6421"
    , [mt|HODL: Hold On for Dear Life|]
        ~: "05010000001b484f444c3a20486f6c64204f6e20666f722044656172204c696665"
    , [mt|\n|]
        ~: "0501000000010a"
    ]

bytesTest :: Spec
bytesTest =
  packSpec
    @ByteString
    [ "000123" ~: "050a00000006303030313233"
    , "A rose by any other name would smell as sweet"
        ~: "050a0000002d4120726f736520627920616e79206f74686572206e616\
           \d6520776f756c6420736d656c6c206173207377656574"
    ]

mutezTest :: Spec
mutezTest =
  packSpec
    @Mutez
    [ (unsafeMkMutez 0  , "050000")
    , (unsafeMkMutez 1  , "050001")
    , (unsafeMkMutez 63 , "05003f")
    , (unsafeMkMutez 64 , "05008001")
    , (unsafeMkMutez 65 , "05008101")
    , (unsafeMkMutez 127, "0500bf01")
    , (unsafeMkMutez 128, "05008002")
    , (unsafeMkMutez 129, "05008102")
    , (unsafeMkMutez 191, "0500bf02")
    , (unsafeMkMutez 192, "05008003")
    , (unsafeMkMutez 193, "05008103")
    ]

boolTest :: Spec
boolTest =
  packSpec
    @Bool
    [ (True , "05030a")
    , (False, "050303")
    ]

keyHashTest :: Spec
keyHashTest = do
  packSpec
    @KeyHash
    [ ( leftToShowPanic $ parseKeyHash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
      , "050a000000150002298c03ed7d454a101eb7022bc95f7e5f41ac78"
      )
    ]

timestampTest :: Spec
timestampTest = do
  packSpec @Timestamp $ convertTimestamps
    [ (205027200, "050080dec3c301")
    , (1552564995, "0500838cd2c80b")
    ]
  where
    convertTimestamps = map . first $ timestampFromSeconds @Int

addressTest :: Spec
addressTest = do
  packSpec @Address $ parseAddrs
    [ ( "tz1PYgf9fBGLXvwx8ag8sdwjLJzmyGdNiswM"
      , "050a0000001600002addb327dbca405f07aeef318bba0ec2f714a755"
      )
    , ( "tz1Z1nn9Y7vzyvtf6rAYMPhPNGqMJXw88xGH"
      , "050a00000016000092b72c0fa1064331a641131f572e7f2abb9a890b"
      )
    , ( "tz1Z1nn9Y7vzyvtf6rAYMPhPNGqMJXw88xGH"
      , "050a00000016000092b72c0fa1064331a641131f572e7f2abb9a890b"
      )
    , ( "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"
      , "050a0000001601122d038abd69be91b4b6803f2f098a088e259e7200"
      )
    , ( "KT1NSrmSJrSueZiWPKrcAUScYr6k2BkUVALr"
      , "050a00000016019812c669d9e8ff1a61bf8c57e33b955f074d832600"
      )
    ]
  where
    parseAddrs = map $ first unsafeParseAddress

keyTest :: Spec
keyTest =
  packSpecManual "key" VKey
    [ ( leftToShowPanic $
          parsePublicKey "edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH"
      , "050a00000021009a85e0f3f47852869ae667adc3b03a20fa9f324d046174dff6834e7d1fab0e8d"
      )
    ]

unitTest :: Spec
unitTest =
  packSpec
    @()
    [() ~: "05030b"]

signatureTest :: Spec
signatureTest =
  packSpecManual "signature" VSignature
    [ ( leftToShowPanic $
          parseSignature "edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8\
                         \ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb"
      , "050a0000004091ac1e7fd668854fc7a40feec4034e42c06c068cce10622c607fda\
        \232db34c8cf5d8da83098dd891cd4cb4299b3fa0352ae323ad99b24541e54b9188\
        \8fdc8201"
      )
    ]

optionTest :: Spec
optionTest = do
  packSpec
    @(Maybe Integer)
    [ Just 123 ~: "05050900bb01"
    , Nothing ~: "050306"
    ]
  packSpec
    @(Maybe MText)
    [ Just [mt|Goodnight World!|]
        ~: "0505090100000010476f6f646e6967687420576f726c6421"
    ]

listTest :: Spec
listTest =
  packSpec
    @[Integer]
    [ [] ~: "050200000000"
    , [1] ~: "0502000000020001"
    , [1..3] ~: "050200000006000100020003"
    ]

setTest :: Spec
setTest =
  packSpec
    @(Set Integer)
    [ [] ~: "050200000000"
    , [1] ~: "0502000000020001"
    , [0, 10, 24, 35, 100, 1000] ~: "05020000000e0000000a0018002300a40100a80f"
    ]

contractTest :: Spec
contractTest = do
  packSpecManual "contract" (VContract @'TUnit) $ parseAddrs
    [ "tz1PYgf9fBGLXvwx8ag8sdwjLJzmyGdNiswM"
        ~: "050a0000001600002addb327dbca405f07aeef318bba0ec2f714a755"
    , "tz1Z1nn9Y7vzyvtf6rAYMPhPNGqMJXw88xGH"
        ~: "050a00000016000092b72c0fa1064331a641131f572e7f2abb9a890b"
    ]

  packSpecManual "contract" (VContract @('Tc 'CInt)) $ parseAddrs
    [ "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"
        ~: "0x050a0000001601122d038abd69be91b4b6803f2f098a088e259e7200"
    ]
  where
    parseAddrs = map $ first unsafeParseAddress

pairTest :: Spec
pairTest = do
  packSpec
    @(MText, Integer)
    [ ([mt|Good Night!|], 5) ~: "050707010000000b476f6f64204e69676874210005"
    ]
  packSpec
    @(Integer, (Integer, MText))
    [ (120, (5, [mt|What is that?|]))
        ~: "05070700b80107070005010000000d5768617420697320746861743f"
    ]

orTest :: Spec
orTest =
  packSpec
    @(Either MText Bool)
    [ Left [mt|Error|] ~: "05050501000000054572726f72"
    , Right True ~: "050508030a"
    ]

mapTest :: Spec
mapTest = do
  packSpec
    @(Map Integer MText)
    [ [] ~: "050200000000"
    , [(0, [mt|Hello|]), (1, [mt|Goodbye|]), (2, [mt|Goodnight|])]
        ~: "05020000003007040000010000000548656c6c6f07040001010000000\
            \7476f6f64627965070400020100000009476f6f646e69676874"
    ]

  packSpec
    @(Map MText (Integer, Bool))
    [ [ ([mt|Tudor|], (123, True))
      , ([mt|Lancaster|], (22323, False))
      , ([mt|Stuart|], (-832988, True))
      ] ~: "050200000040070401000000094c616e636173746572070700b3dc0203\
           \0307040100000006537475617274070700dcd765030a07040100000005\
           \5475646f72070700bb01030a"
    ]

instrTest :: Spec
instrTest = do
  -- Values we compare against are produced with command
  -- ./alphanet.sh client hash data '{ $instrs }' of type 'lambda int int'

  parsePackSpec @('Tc 'CInt) @('Tc 'CInt) "instr"
    [ "{ }"
       ~: "0x050200000000"
    , "{ PUSH int 1; DROP }"
       ~: "0x0502000000080743035b00010320"
    , "{ DUP; SWAP; DROP }"
       ~: "0x0502000000060321034c0320"
    , "{ UNIT; DROP }"
       ~: "0x050200000004034f0320"
    , "{ PUSH int 1; SOME; IF_NONE {} {DROP} }"
       ~: "0x0502000000160743035b00010346072f020000000002000000020320"
    , "{ PUSH int 1; SOME; IF_SOME {DROP} {} }"
       ~: "0x05020000001b0743035b00010346020000000e072f020000000002000000020320"
    , "{ NONE int; DROP }"
       ~: "0x050200000006053e035b0320"
    , "{ PUSH int 1; PAIR; CAR }"
       ~: "0x05020000000a0743035b000103420316"
    , "{ LEFT unit; IF_LEFT {} { DROP; PUSH int 1 } }"
       ~: "0x0502000000180533036c072e0200000000020000000803200743035b0001"
    , "{ RIGHT unit; IF_RIGHT {} { DROP; PUSH int 1 } }"
       ~: "0x05020000001d0544036c0200000014072e020000000803200743035b00010200000000"
    , "{ DUP; NIL int; SWAP; CONS; SIZE; DROP }"
       ~: "0x05020000000e0321053d035b034c031b03450320"
    , "{ NIL int; IF_CONS { DROP; DROP } {} }"
       ~: "0x050200000014053d035b072d0200000004032003200200000000"
    , "{ EMPTY_SET int; ITER { DROP } }"
       ~: "0x05020000000d0524035b055202000000020320"
    , "{ EMPTY_MAP int unit; MAP {}; DROP }"
       ~: "0x05020000000f0723035b036c053802000000000320"
    , "{ EMPTY_MAP int unit; PUSH int 1; MEM; DROP }"
       ~: "0x0502000000100723035b036c0743035b000103390320"
    , "{ EMPTY_MAP int unit; PUSH int 1; GET; DROP }"
       ~: "0x0502000000100723035b036c0743035b000103290320"
    , "{ EMPTY_MAP int unit; NONE unit; PUSH int 1; UPDATE; DROP }"
       ~: "0x0502000000140723035b036c053e036c0743035b000103500320"
    , "{ PUSH bool True; IF {} {} }"
       ~: "0x05020000001207430359030a072c02000000000200000000"
    , "{ PUSH bool True; LOOP { PUSH bool False } }"
       ~: "0x05020000001307430359030a05340200000006074303590303"
    , "{ PUSH (or int int) (Left 1); LOOP_LEFT { RIGHT int }; DROP }"
       ~: "0x05020000001907430764035b035b05050001055302000000040544035b0320"
    , "{ LAMBDA int int { PUSH int 1; DROP }; SWAP; EXEC }"
       ~: "0x05020000001f093100000011035b035b02000000080743035b0001032000000000034c0326"
    , "{ DIP {} }"
       ~: "0x050200000007051f0200000000"
    , "{ FAILWITH }"
       ~: "0x0502000000020327"
    , "{ CAST int }"
       ~: "0x0502000000040557035b"
    , "{ RENAME }"
       ~: "0x0502000000020358"
    , "{ DUP; PACK; UNPACK unit; DROP }"
       ~: "0x05020000000a0321030c050d036c0320"
    , "{ PUSH string \"\"; DUP; CONCAT; DROP }"
       ~: "0x05020000000f0743036801000000000321031a0320"
    , "{ NIL string; CONCAT; DROP }"
       ~: "0x050200000008053d0368031a0320"
    , "{ PUSH string \"\"; PUSH nat 1; PUSH nat 2; SLICE; DROP }"
       ~: "0x050200000019074303680100000000074303620001074303620002036f0320"
    , "{ PUSH int 1; ISNAT; DROP }"
       ~: "0x05020000000a0743035b000103560320"
      -- Arithmetic instructions are below
    , "{ PUSH nat 1; INT; DROP }"
       ~: "0x05020000000a07430362000103300320"
      -- SELF cannot appear in lambda
      -- CONTRACT - IMPLICIT_ACCOUNT go below
    , "{ NOW; DROP }"
      ~: "0x05020000000403400320"
    , "{ AMOUNT; DROP }"
      ~: "0x05020000000403130320"
    , "{ BALANCE; DROP }"
      ~: "0x05020000000403150320"
      -- CHECK_SIGNATURE goes below
    , "{ PUSH bytes 0x; SHA256; DROP }"
      ~: "0x05020000000d074303690a00000000030f0320"
    , "{ PUSH bytes 0x; SHA512; DROP }"
      ~: "0x05020000000d074303690a0000000003100320"
    , "{ PUSH bytes 0x; BLAKE2B; DROP }"
      ~: "0x05020000000d074303690a00000000030e0320"
      -- HASH_KEY goes below
    , "{ STEPS_TO_QUOTA; DROP }"
      ~: "0x050200000004034a0320"
    , "{ SOURCE; DROP }"
      ~: "0x05020000000403470320"
    , "{ SENDER; DROP }"
      ~: "0x05020000000403480320"
      -- ADDRESS goes below
    ]

  parsePackSpec @'TUnit @'TUnit "arith instr"
    [ "{ PUSH int 1; PUSH int 2; ADD; DROP }"
       ~: "0x0502000000100743035b00010743035b000203120320"
    , "{ PUSH int 1; PUSH int 2; SUB; DROP }"
       ~: "0x0502000000100743035b00010743035b0002034b0320"
    , "{ PUSH int 1; PUSH int 2; MUL; DROP }"
       ~: "0x0502000000100743035b00010743035b0002033a0320"
    , "{ PUSH int 1; PUSH int 2; EDIV; DROP }"
       ~: "0x0502000000100743035b00010743035b000203220320"
    , "{ PUSH int 1; ABS; DROP }"
       ~: "0x05020000000a0743035b000103110320"
    , "{ PUSH int 1; NEG; DROP }"
       ~: "0x05020000000a0743035b0001033b0320"
    , "{ PUSH nat 1; PUSH nat 2; LSL; DROP }"
       ~: "0x05020000001007430362000107430362000203350320"
    , "{ PUSH nat 1; PUSH nat 2; LSR; DROP }"
       ~: "0x05020000001007430362000107430362000203360320"
    , "{ PUSH nat 1; PUSH nat 2; OR; DROP }"
       ~: "0x05020000001007430362000107430362000203410320"
    , "{ PUSH nat 1; PUSH nat 2; XOR; DROP }"
       ~: "0x05020000001007430362000107430362000203510320"
    , "{ PUSH int 1; NOT; DROP }"
       ~: "0x05020000000a0743035b0001033f0320"
    , "{ PUSH nat 1; PUSH nat 2; COMPARE; DROP }"
       ~: "0x05020000001007430362000107430362000203190320"
    , "{ PUSH int 1; EQ; DROP }"
       ~: "0x05020000000a0743035b000103250320"
    , "{ PUSH int 1; NEQ; DROP }"
       ~: "0x05020000000a0743035b0001033c0320"
    , "{ PUSH int 1; LT; DROP }"
       ~: "0x05020000000a0743035b000103370320"
    , "{ PUSH int 1; GT; DROP }"
       ~: "0x05020000000a0743035b0001032a0320"
    , "{ PUSH int 1; LE; DROP }"
       ~: "0x05020000000a0743035b000103320320"
    , "{ PUSH int 1; GE; DROP }"
       ~: "0x05020000000a0743035b000103280320"
    ]

  parsePackSpec @('Tc 'CAddress) @'TUnit "instrs address-related"
    [ "{ CONTRACT unit; DROP; PUSH unit Unit }"
       ~: "0x05020000000c0555036c03200743036c030b"
    ]

  parsePackSpec @('TContract 'TUnit) @'TUnit "instrs contract-related"
    [ "{ PUSH mutez 5; PUSH unit Unit; TRANSFER_TOKENS; DROP; PUSH unit Unit }"
       ~: "0x0502000000160743036a00050743036c030b034d03200743036c030b"
    , "{ ADDRESS; DROP; PUSH unit Unit }"
       ~: "0x05020000000a035403200743036c030b"
    ]

  parsePackSpec @('Tc 'CKeyHash) @'TUnit "instrs key-hash-related"
    [ "{ SOME; SET_DELEGATE; DROP; PUSH unit Unit }"
       ~: "0x05020000000c0346034e03200743036c030b"
    , "{ DUP; DIP{ SOME; DIP{ PUSH mutez 5; PUSH bool True } }; \
        \CREATE_ACCOUNT; DROP; DROP; PUSH unit Unit \
      \}"
       ~: "0x05020000002a0321051f02000000150346051f020000000c0743036a00\
          \0507430359030a031c032003200743036c030b"
    , "{ DUP; DIP{ SOME; DIP{ PUSH unit Unit; PUSH mutez 5; PUSH bool True; DUP } }; \
       \ CREATE_CONTRACT{ \
       \   parameter unit; \
       \   storage unit; \
       \   code { DROP; UNIT; NIL operation; PAIR } \
       \  }; \
       \ DROP; DROP; PUSH unit Unit \
       \}"
       ~: "0x0502000000500321051f020000001d0346051f02000000140743036c030\
          \b0743036a000507430359030a0321051d02000000190500036c0501036c050\
          \2020000000a0320034f053d036d0342032003200743036c030b"
    , "{ IMPLICIT_ACCOUNT; DROP; PUSH unit Unit }"
       ~: "0x05020000000a031e03200743036c030b"
    ]

  parsePackSpec @'TKey @('Tc 'CKeyHash) "instrs public-key-related"
    [ "{ HASH_KEY }"
       ~: "0x050200000002032b"
    ]

  parsePackSpec @('TPair 'TSignature 'TKey) @('Tc 'CBool) "instrs public-key-related"
    [ "{ DIP{ PUSH bytes 0x }; DUP; DIP {CAR}; CDR; CHECK_SIGNATURE }"
       ~: "0x05020000001f051f0200000009074303690a000000000321051f020000\
          \0002031603170318"
    ]

typesTest :: Spec
typesTest = do
  -- Bytes we compare agains are produced with command
  -- ./alphanet.sh client hash data '{ LAMBDA ($ty) ($ty) {}; DROP }' of type 'lambda unit unit' /
  --     | tr -d '\n' | awk '{ print $45 }' | sed 's/Hash://'

  parsePackSpec @'TUnit @'TUnit "types"
    [ lambdaWrap "int"
        ~: "0x050200000015093100000009035b035b0200000000000000000320"
    , lambdaWrap "nat"
        ~: "0x050200000015093100000009036203620200000000000000000320"
    , lambdaWrap "string"
        ~: "0x050200000015093100000009036803680200000000000000000320"
    , lambdaWrap "bytes"
        ~: "0x050200000015093100000009036903690200000000000000000320"
    , lambdaWrap "mutez"
        ~: "0x050200000015093100000009036a036a0200000000000000000320"
    , lambdaWrap "bool"
        ~: "0x050200000015093100000009035903590200000000000000000320"
    , lambdaWrap "key_hash"
        ~: "0x050200000015093100000009035d035d0200000000000000000320"
    , lambdaWrap "timestamp"
        ~: "0x050200000015093100000009036b036b0200000000000000000320"
    , lambdaWrap "address"
        ~: "0x050200000015093100000009036e036e0200000000000000000320"
    , lambdaWrap "key"
        ~: "0x050200000015093100000009035c035c0200000000000000000320"
    , lambdaWrap "unit"
        ~: "0x050200000015093100000009036c036c0200000000000000000320"
    , lambdaWrap "signature"
        ~: "0x050200000015093100000009036703670200000000000000000320"
    , lambdaWrap "option unit"
        ~: "0x05020000001909310000000d0563036c0563036c0200000000000000000320"
    , lambdaWrap "set int"
        ~: "0x05020000001909310000000d0566035b0566035b0200000000000000000320"
    , lambdaWrap "operation"
        ~: "0x050200000015093100000009036d036d0200000000000000000320"
    , lambdaWrap "contract unit"
        ~: "0x05020000001909310000000d055a036c055a036c0200000000000000000320"
    , lambdaWrap "pair unit int"
        ~: "0x05020000001d0931000000110765036c035b0765036c035b0200000000000000000320"
    , lambdaWrap "or unit int"
        ~: "0x05020000001d0931000000110764036c035b0764036c035b0200000000000000000320"
    , lambdaWrap "lambda unit int"
        ~: "0x05020000001d093100000011075e036c035b075e036c035b0200000000000000000320"
    , lambdaWrap "map int unit"
        ~: "0x05020000001d0931000000110760035b036c0760035b036c0200000000000000000320"
    , lambdaWrap "big_map int unit"
        ~: "0x05020000001d0931000000110761035b036c0761035b036c0200000000000000000320"
    ]
  where
    lambdaWrap ty = "{ LAMBDA " <> ty <> " " <> ty <> " {}; DROP }"

unpackNegTest :: Spec
unpackNegTest = do
  describe "Bad entries order" $ do
    unpackNegSpec @('TSet 'CInt) "Unordered set elements"
      "0x050200000006000300020001"  -- { 3; 2; 1 }
    unpackNegSpec @('TMap 'CInt $ 'Tc 'CInt) "Unordered map elements"
      "0x05020000000c070400020006070400010007"  -- { Elt 2 6; Elt 1 7 }

  describe "Wron length specified" $ do
    unpackNegSpec @('TList $ 'Tc 'CInt) "Too few list length"
      "0x05020000000300010002"  -- { 1; 2 }
    unpackNegSpec @('TList $ 'Tc 'CInt) "Too big list length"
      "0x05020000000500010002"  -- { 1; 2 }
    unpackNegSpec @('TList $ 'Tc 'CInt) "Wrong bytes length"
      "0x050b000000021234"  -- 0x1234

  describe "Type check failures" $ do
    unpackNegSpec @('TUnit) "Value type mismatch"
      "0x050008"  -- 8
    unpackNegSpec @('TLambda 'TUnit 'TKey) "Lambda type mismatch"
      "0x050200000000"  -- {}
    unpackNegSpec @('TLambda 'TUnit 'TKey) "Lambda too large output stack size"
      "0x0502000000060743035b0005"  -- {PUSH int 5}
    unpackNegSpec @('TLambda 'TUnit 'TKey) "Lambda empty output stack size"
      "0x0502000000020320"  -- {DROP}
