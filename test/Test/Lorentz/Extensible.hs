module Test.Lorentz.Extensible
  ( test_Extensible
  ) where

import Data.Vinyl.Core (Rec(..))
import Test.HUnit ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Lorentz (toVal)
import Lorentz.Base
import Lorentz.Extensible
import Michelson.Interpret.Pack
import Michelson.Interpret.Unpack
import Michelson.Test.Dummy

data SumType
  = Ctor0 Natural
  | Ctor1
  | Ctor2 (Integer, Integer)
  deriving stock (Generic, Eq, Show)

values :: [SumType]
values =
  [ Ctor0 42
  , Ctor1
  , Ctor2 (-100, 500)
  ]

test_Extensible :: [TestTree]
test_Extensible =
  [ testCase "toExtVal conversion" $ do
      fmap toExtVal values @?=
        [ Extensible (0, packValue' $ toVal @Natural 42)
        , Extensible (1, packValue' $ toVal ())
        , Extensible (2, packValue' $ toVal ints)
        ]
  , testCase "fromExtVal conversion" $ do
      let extValues = [ Extensible (0, packValue' $ toVal @Natural 42)
                      , Extensible (1, packValue' $ toVal ())
                      , Extensible (2, packValue' $ toVal ints)
                      ]
      fmap (fromExtVal dummyUnpackEnv) extValues @?= fmap Right values
  , testCase "fromExtVal failure" $ do
      let invalidCtor = Extensible (3, packValue' $ toVal ())
      let invalidArg = Extensible (2, packValue' $ toVal ())
      fromExtVal @SumType dummyUnpackEnv invalidCtor
        @?= Left (ConstructorIndexNotFound 3)
      fromExtVal @SumType dummyUnpackEnv invalidArg
        @?= Left ArgumentUnpackFailed
  , testCase "wrapExt" $ do
      wrapIntoCtor2 ints @?= (Right $ Extensible (2, packValue' $ toVal ints))
  , testCase "Roundtrip" $ do
      fmap roundtrip values @?= fmap Right values
  ]
  where
    wrapIntoCtor2 initVal = do
      let initStack = (Identity initVal :& RNil)
      resStack <- interpretLorentzInstr dummyContractEnv wrapCode initStack
      let Identity res :& RNil = resStack
      return res

ints :: (Integer, Integer)
ints = (-100, 500)

wrapCode :: '[(Integer, Integer)] :-> '[Extensible SumType]
wrapCode = wrapExt #cCtor2

roundtrip :: (ExtVal value) => value -> Either ExtConversionError value
roundtrip value =
  fromExtVal dummyUnpackEnv $ toExtVal value
