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
  , roundtripSpec
  , roundtripSpecSTB
  , aesonRoundtrip

  -- * 'Property' helpers
  , failedProp
  , qcIsLeft
  , qcIsRight

  -- * 'Gen' helpers
  , runGen
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Typeable (typeRep)
import Fmt (Buildable, pretty)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, (===))
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Property (Result(..), failed, property)
import Test.QuickCheck.Random (mkQCGen)
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

-- | This 'Spec' contains a property based test for conversion from
-- some @x@ to some @y@ and back to @x@ (it should successfully return
-- the initial @x@).
roundtripSpec ::
     forall x y err.
     ( Show x
     , Typeable x
     , Arbitrary x
     , Eq x
     , Show err
     , Eq err
     )
  => (x -> y)
  -> (y -> Either err x)
  -> Spec
roundtripSpec xToY yToX = prop typeName check
  where
    typeName = show $ typeRep (Proxy @x)
    check :: x -> Property
    check x = yToX (xToY x) === Right x

-- | Version of 'roundtripSpec' which shows values using 'Buildable' instance.
roundtripSpecSTB ::
     forall x y err.
     ( Show (ShowThroughBuild x)
     , Typeable x
     , Arbitrary x
     , Eq x
     , Show (ShowThroughBuild err)
     , Eq err
     )
  => (x -> y)
  -> (y -> Either err x)
  -> Spec
roundtripSpecSTB xToY yToX = roundtripSpec (xToY . unSTB) (bimap STB STB . yToX)

aesonRoundtrip ::
     forall x. (Show (ShowThroughBuild x), ToJSON x, FromJSON x, Typeable x, Arbitrary x, Eq x)
  => Spec
aesonRoundtrip = roundtripSpecSTB (Aeson.encode @x) Aeson.eitherDecode

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

-- | A 'Property' that always failes with given message.
failedProp :: Text -> Property
failedProp r = property $ failed { reason = toString r }

-- | The 'Property' holds on `Left a`.
qcIsLeft :: Show b => Either a b -> Property
qcIsLeft (Left _)  = property True
qcIsLeft (Right x) = failedProp $ "expected Left, got Right (" <> show x <> ")"

-- | The 'Property' holds on `Right b`.
qcIsRight :: Show a => Either a b -> Property
qcIsRight (Right _) = property True
qcIsRight (Left x)  = failedProp $ "expected Right, got Left (" <> show x <> ")"

----------------------------------------------------------------------------
-- Gen
----------------------------------------------------------------------------

-- | Get something out of a quickcheck 'Gen' without having to do IO
runGen :: Gen a -> a
runGen g = unGen g (mkQCGen 31415926) 30
