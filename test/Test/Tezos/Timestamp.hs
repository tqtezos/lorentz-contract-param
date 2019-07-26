module Test.Tezos.Timestamp
  ( test_TimestampQuote
  ) where

import Test.HUnit ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Tezos.Core

test_TimestampQuote :: [TestTree]
test_TimestampQuote =
  [ testCase "Quoter works in simple case" $
      [timestampQuote|2019-07-26T12:09:12Z|]
        @?= timestampFromSeconds 1564142952

  , testCase "Quoter works with surrounding spaces" $
      [timestampQuote| 2019-07-26T12:09:12Z |]
        @?= timestampFromSeconds 1564142952
  ]
