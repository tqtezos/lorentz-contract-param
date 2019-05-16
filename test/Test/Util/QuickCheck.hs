{-
Copyright (c) 2017 IOHK

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

-- | Testing utilities to be used with QuickCheck

module Test.Util.QuickCheck
  ( ShowThroughBuild (..)

  -- * Roundtrip properties
  , roundtripTest
  , roundtripADTTest
  , roundtripTestSTB
  , aesonRoundtrip
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Typeable (typeRep)
import Fmt (Buildable, pretty)
import Test.QuickCheck (Arbitrary, forAll, resize)
import Test.QuickCheck.Arbitrary.ADT
  (ADTArbitrary(..), ConstructorArbitraryPair(..), ToADTArbitrary(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Property, testProperty, (.&&.), (===))
import qualified Text.Show (show)

----------------------------------------------------------------------------
-- 'Show'ing a value though 'Buildable' type class.
-- Useful because QuickCheck uses 'Show'.
----------------------------------------------------------------------------

newtype ShowThroughBuild a = STB
  { unSTB :: a
  } deriving (Eq, Ord, Arbitrary)

instance {-# OVERLAPPABLE #-} Buildable a => Show (ShowThroughBuild a) where
  show = pretty . unSTB

instance Show (ShowThroughBuild ByteString) where
  show = show . unSTB

----------------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------------

-- | This 'TestTree' contains a property based test for conversion from
-- some @x@ to some @y@ and back to @x@ (it should successfully return
-- the initial @x@).
roundtripTest ::
     forall x y err.
     ( Show x
     , Show err
     , Typeable x
     , Arbitrary x
     , Eq x
     , Eq err
     )
  => (x -> y)
  -> (y -> Either err x)
  -> TestTree
roundtripTest xToY yToX = testProperty typeName check
  where
    typeName = show $ typeRep (Proxy @x)
    check :: x -> Property
    check x = yToX (xToY x) === Right x

roundtripADTTest ::
     forall x y err.
     ( Show x
     , Show err
     , Typeable x
     , ToADTArbitrary x
     , Eq x
     , Eq err
     )
  => (x -> y)
  -> (y -> Either err x)
  -> TestTree
roundtripADTTest xToY yToX = testProperty typeName prop
  where
    prop :: Property
    prop =
      forAll (resize 1 $ toADTArbitrary (Proxy @x)) $ \adt ->
        foldr ((.&&.) . check . capArbitrary) z (adtCAPs adt)
    typeName = show $ typeRep (Proxy @x)
    check x = yToX (xToY x) === Right x
    z = True === True

-- | Version of 'roundtripTest' which shows values using 'Buildable' instance.
roundtripTestSTB ::
     forall x y err.
     ( Show (ShowThroughBuild x)
     , Show (ShowThroughBuild err)
     , Typeable x
     , Arbitrary x
     , Eq x
     , Eq err
     )
  => (x -> y)
  -> (y -> Either err x)
  -> TestTree
roundtripTestSTB xToY yToX = roundtripTest (xToY . unSTB) (bimap STB STB . yToX)

aesonRoundtrip ::
     forall x. (Show (ShowThroughBuild x), ToJSON x, FromJSON x, Typeable x, Arbitrary x, Eq x)
  => TestTree
aesonRoundtrip = roundtripTestSTB (Aeson.encode @x) Aeson.eitherDecode
