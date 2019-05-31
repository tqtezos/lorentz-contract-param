{-# OPTIONS_GHC -Wno-orphans #-}

module Util.Test.Arbitrary
  ( runGen
  ) where

import Prelude hiding (EQ, GT, LT)

import Test.QuickCheck (Arbitrary(..), Gen, choose, elements, frequency, oneof, suchThatMap, vector)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary(..))
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Random (mkQCGen)

import Michelson.ErrorPos (InstrCallStack(..), LetName(..), Pos(..), SrcPos(..))
import Michelson.Test ()
import Michelson.Untyped
  (Annotation(..), CT(..), Comparable(..), Contract'(..), Elt(..), ExpandedExtInstr,
  ExpandedOp(..), ExtInstrAbstract(..), FieldAnn, InstrAbstract(..), InternalByteString(..),
  StackTypePattern(..), T(..), TyVar(..), Type(..), TypeAnn, Value'(..), Var(..), VarAnn)
import Tezos.Core (Mutez(..))

instance Arbitrary InternalByteString where
  arbitrary = InternalByteString <$> arbitrary

instance Arbitrary Var where
  arbitrary = Var <$> arbitrary

instance Arbitrary TyVar where
  arbitrary = oneof [VarID <$> arbitrary, TyCon <$> arbitrary]

instance Arbitrary StackTypePattern where
  arbitrary = oneof [pure StkEmpty, pure StkRest, StkCons <$> arbitrary <*> arbitrary]

-- TODO extend Arbitrary ExpandedExtInstr with other constructors
instance Arbitrary ExpandedExtInstr where
  arbitrary = oneof [STACKTYPE <$> arbitrary]

instance ToADTArbitrary Pos
instance Arbitrary Pos where
  arbitrary = Pos <$> arbitrary

instance ToADTArbitrary SrcPos
instance Arbitrary SrcPos where
  arbitrary = liftA2 SrcPos arbitrary arbitrary

instance ToADTArbitrary LetName
instance Arbitrary LetName where
  arbitrary = LetName <$> arbitrary

instance ToADTArbitrary InstrCallStack
instance Arbitrary InstrCallStack where
  arbitrary = liftA2 InstrCallStack (vector 2) arbitrary

instance ToADTArbitrary ExpandedOp
instance Arbitrary ExpandedOp where
  arbitrary = liftA2 WithSrcEx arbitrary (PrimEx <$> arbitrary)

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

smallList :: Arbitrary a => Gen [a]
smallList = smallSize >>= vector

smallList1 :: Arbitrary a => Gen (NonEmpty a)
smallList1 = smallList `suchThatMap` nonEmpty

instance (Arbitrary op, ToADTArbitrary op) => ToADTArbitrary (Contract' op)
instance (Arbitrary op) => Arbitrary (Contract' op) where
  arbitrary = Contract <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary op, ToADTArbitrary op, Arbitrary (ExtInstrAbstract op)) => ToADTArbitrary (InstrAbstract op)
instance (Arbitrary op, Arbitrary (ExtInstrAbstract op)) => Arbitrary (InstrAbstract op) where
  arbitrary =
    oneof
      [ EXT <$> arbitrary
      , pure DROP
      , DUP <$> arbitrary
      , pure SWAP
      , PUSH <$> arbitrary <*> arbitrary <*> arbitrary
      , SOME <$> arbitrary <*> arbitrary <*> arbitrary
      , NONE <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , UNIT <$> arbitrary <*> arbitrary
      , IF_NONE <$> smallList <*> smallList
      , PAIR <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , CAR <$> arbitrary <*> arbitrary
      , CDR <$> arbitrary <*> arbitrary
      , LEFT <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , RIGHT <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , IF_LEFT <$> smallList <*> smallList
      , NIL <$> arbitrary <*> arbitrary <*> arbitrary
      , CONS <$> arbitrary
      , IF_CONS <$> smallList <*> smallList
      , SIZE <$> arbitrary
      , EMPTY_SET <$> arbitrary <*> arbitrary <*> arbitrary
      , EMPTY_MAP <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , MAP <$> arbitrary <*> smallList
      , ITER <$> smallList
      , MEM <$> arbitrary
      , GET <$> arbitrary
      , pure UPDATE
      , IF <$> smallList <*> smallList
      , LOOP <$> smallList
      , LOOP_LEFT <$> smallList
      , LAMBDA <$> arbitrary <*> arbitrary <*> arbitrary <*> smallList
      , EXEC <$> arbitrary
      , DIP <$> smallList
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
      , CREATE_CONTRACT <$> arbitrary <*> arbitrary <*> arbitrary
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

instance (Arbitrary op, ToADTArbitrary op) => ToADTArbitrary (Value' op)
instance (Arbitrary op) => Arbitrary (Value' op) where
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
      , pure ValueNil
      , ValueSeq <$> smallList1
      , ValueMap <$> smallList1
      , ValueLambda <$> smallList1
      ]

instance (Arbitrary op, ToADTArbitrary op) => ToADTArbitrary (Elt op)
instance (Arbitrary op) => Arbitrary (Elt op) where
  arbitrary = Elt <$> arbitrary <*> arbitrary

instance ToADTArbitrary Type
instance Arbitrary Type where
  arbitrary = Type <$> arbitrary <*> arbitrary

-- | @getRareT k@ generates 'T' producing anything big once per @1 / (k + 1)@
-- invocation.
genRareType :: Word -> Gen Type
genRareType k = Type <$> genRareT k <*> arbitrary

instance ToADTArbitrary T
instance Arbitrary T where
  arbitrary =
    oneof
      [ Tc <$> arbitrary
      , pure TKey
      , pure TUnit
      , pure TSignature
      , TOption <$> arbitrary <*> arbitrary
      , TList <$> arbitrary
      , TSet <$> arbitrary
      , pure TOperation
      , TContract <$> arbitrary
      , TPair <$> arbitrary <*> arbitrary <*> genRareType 5 <*> genRareType 5
      , TOr <$> arbitrary <*> arbitrary <*> genRareType 5 <*> genRareType 5
      , TLambda <$> genRareType 5 <*> genRareType 5
      , TMap <$> arbitrary <*> arbitrary
      , TBigMap <$> arbitrary <*> arbitrary
      ]

-- | @getRareT k@ generates 'Type' producing anything big once per @1 / (k + 1)@
-- invocation.
--
-- Useful to avoid exponensial growth.
genRareT :: Word -> Gen T
genRareT k = frequency [(1, arbitrary), (fromIntegral k, pure TUnit)]

instance ToADTArbitrary CT
instance Arbitrary CT where
  arbitrary =  elements [minBound .. maxBound]

instance ToADTArbitrary Comparable
instance Arbitrary Comparable where
  arbitrary = Comparable <$> arbitrary <*> arbitrary

-- | Run given generator deterministically.
runGen :: Int -> Gen a -> a
runGen seed gen = unGen gen (mkQCGen seed) 10
