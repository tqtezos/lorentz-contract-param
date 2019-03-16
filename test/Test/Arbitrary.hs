{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Arbitrary () where

import Prelude hiding (EQ, GT, LT)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.QuickCheck (Arbitrary(..), Gen, choose, elements, listOf, oneof, vector)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary(..))

import Michelson.Untyped
  (Annotation(..), CT(..), Comparable(..), Contract(..), Elt(..), FieldAnn, InstrAbstract(..),
  InternalByteString(..), Op(..), T(..), Type(..), TypeAnn, Value(..), VarAnn)
import Morley.Test ()
import Morley.Types (NopInstr(..), StackTypePattern(..), TyVar(..), Var(..))
import Tezos.Core (Mutez(..), Timestamp(..), timestampFromSeconds)

instance Arbitrary InternalByteString where
  arbitrary = InternalByteString . T.encodeUtf8 . T.pack <$> listOf arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Var where
  arbitrary = Var <$> arbitrary

instance Arbitrary TyVar where
  arbitrary = oneof [VarID <$> arbitrary, TyCon <$> arbitrary]

instance Arbitrary StackTypePattern where
  arbitrary = oneof [pure StkEmpty, pure StkRest, StkCons <$> arbitrary <*> arbitrary]

-- TODO extend Arbitrary NopInstr with other constructors
instance Arbitrary NopInstr where
  arbitrary = oneof [STACKTYPE <$> arbitrary]

instance (Arbitrary nop, ToADTArbitrary nop) => ToADTArbitrary (Op nop)
instance Arbitrary nop => Arbitrary (Op nop) where
  arbitrary = Op <$> arbitrary

-- TODO: this `Timestamp` gen differs from `Gen (CVal 'T_timestamp)` from `Morley.Test`.
instance Arbitrary Timestamp where
  arbitrary = timestampFromSeconds @Word <$> arbitrary

instance ToADTArbitrary Mutez

instance ToADTArbitrary TypeAnn
instance Arbitrary TypeAnn where
  arbitrary = Annotation <$> arbitrary

instance ToADTArbitrary FieldAnn
instance Arbitrary FieldAnn where
  arbitrary = Annotation <$> arbitrary

instance ToADTArbitrary VarAnn
instance Arbitrary VarAnn where
  arbitrary = Annotation <$> arbitrary

smallSize :: Gen Int
smallSize = choose (0, 3)

instance (Arbitrary op, ToADTArbitrary op) => ToADTArbitrary (Contract op)
instance (Arbitrary op) => Arbitrary (Contract op) where
  arbitrary = Contract <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary op, ToADTArbitrary op
         , ToADTArbitrary nop, Arbitrary nop) => ToADTArbitrary (InstrAbstract nop op)
instance (Arbitrary op, Arbitrary nop) => Arbitrary (InstrAbstract nop op) where
  arbitrary =
    oneof
      [ NOP <$> arbitrary
      , pure DROP
      , DUP <$> arbitrary
      , pure SWAP
      , PUSH <$> arbitrary <*> arbitrary <*> arbitrary
      , SOME <$> arbitrary <*> arbitrary <*> arbitrary
      , NONE <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , UNIT <$> arbitrary <*> arbitrary
      , (do size1 <- smallSize
            size2 <- smallSize
            l1 <- vector size1
            l2 <- vector size2
            pure $ IF_NONE l1 l2
        )
      , PAIR <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , CAR <$> arbitrary <*> arbitrary
      , CDR <$> arbitrary <*> arbitrary
      , LEFT <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , RIGHT <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , (do size1 <- smallSize
            size2 <- smallSize
            l1 <- vector size1
            l2 <- vector size2
            pure $ IF_LEFT l1 l2
        )
      , (do size1 <- smallSize
            size2 <- smallSize
            l1 <- vector size1
            l2 <- vector size2
            pure $ IF_RIGHT l1 l2
        )
      , NIL <$> arbitrary <*> arbitrary <*> arbitrary
      , CONS <$> arbitrary
      , (do size1 <- smallSize
            size2 <- smallSize
            l1 <- vector size1
            l2 <- vector size2
            pure $ IF_CONS l1 l2
        )
      , SIZE <$> arbitrary
      , EMPTY_SET <$> arbitrary <*> arbitrary <*> arbitrary
      , EMPTY_MAP <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , (do size1 <- smallSize
            l1 <- vector size1
            MAP <$> arbitrary <*> pure l1
        )
      , (do size1 <- smallSize
            l1 <- vector size1
            pure $ ITER l1
        )
      , MEM <$> arbitrary
      , GET <$> arbitrary
      , pure UPDATE
      , (do size1 <- smallSize
            size2 <- smallSize
            l1 <- vector size1
            l2 <- vector size2
            pure $ IF l1 l2
        )
      , (do size1 <- smallSize
            l1 <- vector size1
            pure $ LOOP l1
        )
      , (do size1 <- smallSize
            l1 <- vector size1
            pure $ LOOP_LEFT l1
        )
      , (do size1 <- smallSize
            l1 <- vector size1
            LAMBDA <$> arbitrary <*> arbitrary <*> arbitrary <*> pure l1
        )
      , EXEC <$> arbitrary
      , (do size1 <- smallSize
            l1 <- vector size1
            pure $ DIP l1
        )
      , pure FAILWITH
      , CAST <$> arbitrary <*> arbitrary
      , RENAME <$> arbitrary
      , PACK <$> arbitrary
      , UNPACK <$> arbitrary <*> arbitrary
      , CONCAT <$> arbitrary
      , SLICE <$> arbitrary
      , ISNAT <$> arbitrary
      , ADD <$> arbitrary
      , SUB <$> arbitrary
      , MUL <$> arbitrary
      , EDIV <$> arbitrary
      , ABS <$> arbitrary
      , pure NEG
      , LSL <$> arbitrary
      , LSR <$> arbitrary
      , OR <$> arbitrary
      , AND <$> arbitrary
      , XOR <$> arbitrary
      , NOT <$> arbitrary
      , COMPARE <$> arbitrary
      , EQ <$> arbitrary
      , NEQ <$> arbitrary
      , LT <$> arbitrary
      , GT <$> arbitrary
      , LE <$> arbitrary
      , GE <$> arbitrary
      , INT <$> arbitrary
      , SELF <$> arbitrary
      , CONTRACT <$> arbitrary <*> arbitrary
      , TRANSFER_TOKENS <$> arbitrary
      , SET_DELEGATE <$> arbitrary
      , CREATE_ACCOUNT <$> arbitrary <*> arbitrary
      , CREATE_CONTRACT <$> arbitrary <*> arbitrary
      , CREATE_CONTRACT2 <$> arbitrary <*> arbitrary <*> arbitrary
      , IMPLICIT_ACCOUNT <$> arbitrary
      , NOW <$> arbitrary
      , AMOUNT <$> arbitrary
      , BALANCE <$> arbitrary
      , CHECK_SIGNATURE <$> arbitrary
      , SHA256 <$> arbitrary
      , SHA512 <$> arbitrary
      , BLAKE2B <$> arbitrary
      , HASH_KEY <$> arbitrary
      , STEPS_TO_QUOTA <$> arbitrary
      , SOURCE <$> arbitrary
      , SENDER <$> arbitrary
      , ADDRESS <$> arbitrary
      ]

instance (Arbitrary op, ToADTArbitrary op) => ToADTArbitrary (Value op)
instance (Arbitrary op) => Arbitrary (Value op) where
  arbitrary =
    oneof
      [ ValueInt <$> arbitrary
      , ValueString <$> arbitrary
      , ValueBytes <$> arbitrary
      , pure ValueUnit
      , pure ValueTrue
      , pure ValueFalse
      , ValuePair <$> arbitrary <*> arbitrary
      , ValueLeft <$> arbitrary
      , ValueRight <$> arbitrary
      , ValueSome <$> arbitrary
      , pure ValueNone
      , (do size1 <- smallSize
            l1 <- vector size1
            pure $ ValueSeq l1
        )
      , (do size1 <- smallSize
            l1 <- vector size1
            pure $ ValueMap l1
        )
      , (do size1 <- smallSize
            l1 <- vector size1
            pure $ ValueLambda l1
        )
      ]

instance (Arbitrary op, ToADTArbitrary op) => ToADTArbitrary (Elt op)
instance (Arbitrary op) => Arbitrary (Elt op) where
  arbitrary = Elt <$> arbitrary <*> arbitrary

instance ToADTArbitrary Type
instance Arbitrary Type where
  arbitrary = Type <$> arbitrary <*> arbitrary

instance ToADTArbitrary T
instance Arbitrary T where
  arbitrary =
    oneof
      [ T_comparable <$> arbitrary
      , pure T_key
      , pure T_unit
      , pure T_signature
      , T_option <$> arbitrary <*> arbitrary
      , T_list <$> arbitrary
      , T_set <$> arbitrary
      , pure T_operation
      , T_contract <$> arbitrary
      , T_pair <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , T_or <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , T_lambda <$> arbitrary <*> arbitrary
      , T_map <$> arbitrary <*> arbitrary
      , T_big_map <$> arbitrary <*> arbitrary
      ]

instance ToADTArbitrary CT
instance Arbitrary CT where
  arbitrary =  elements [minBound .. maxBound]

instance ToADTArbitrary Comparable
instance Arbitrary Comparable where
  arbitrary = Comparable <$> arbitrary <*> arbitrary
