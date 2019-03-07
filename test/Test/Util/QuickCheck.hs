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

  -- * Formatting/parsing properties
  , formattingRoundtrip
  , aesonRoundtrip

  -- * 'Property' helpers
  , failedProp
  ) where

import Formatting (build, formatToString)
import Formatting.Buildable (Buildable)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Property (Result(..), failed, property)
import Text.Show (show)
import Prelude hiding (show)

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Typeable (typeRep)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===))

----------------------------------------------------------------------------
-- 'Show'ing a value though 'Buildable' type class.
-- Useful because QuickCheck uses 'Show'.
----------------------------------------------------------------------------

newtype ShowThroughBuild a = STB
  { unSTB :: a
  } deriving (Eq, Ord, Arbitrary)

instance Buildable a => Show (ShowThroughBuild a) where
  show = formatToString build . unSTB

----------------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------------

formattingRoundtrip ::
     forall x err. (Buildable x, Typeable x, Arbitrary x, Buildable err, Eq x, Eq err)
  => (x -> Text)
  -> (Text -> Either err x)
  -> Spec
formattingRoundtrip format parse = prop typeName check
  where
    typeName = show $ typeRep (Proxy @x)
    check :: ShowThroughBuild x -> Property
    check (STB x) = bimap STB STB (parse (format x)) === Right (STB x)

aesonRoundtrip ::
     forall x. (Buildable x, ToJSON x, FromJSON x, Typeable x, Arbitrary x, Eq x)
  => Spec
aesonRoundtrip =
  formattingRoundtrip
    (decodeUtf8 . Aeson.encode @x)
    (Aeson.eitherDecode . encodeUtf8)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

-- | A 'Property' that always failes with given message.
failedProp :: Text -> Property
failedProp r = property $ failed { reason = toString r }
