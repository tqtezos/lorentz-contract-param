-- | HUnit support for tasty.
--
-- We don't use `tasty-hunit`, because it doesn't interoperate properly with
-- other HUnit-based code.
-- Specifically, it defines its own `HUnitFailure` type and catches exceptions
-- of this type. It doesn't catch HUnit's `HUnitFailure`, so they are not
-- pretty-printed.

module Test.Tasty.HUnit
  ( testCase
  , testCaseInfo
  , testCaseSteps
  ) where

import GHC.Stack (prettySrcLoc)
import Test.HUnit (Assertion)
import Test.HUnit.Lang (HUnitFailure(..), formatFailureReason)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Providers (IsTest(..), singleTest, testFailed, testPassed)

-- | Turn an 'Assertion' into a tasty test case
testCase :: TestName -> Assertion -> TestTree
testCase name = singleTest name . TestCaseWrapper . fmap (const "")

testCaseInfo :: TestName -> IO String -> TestTree
testCaseInfo name = singleTest name . TestCaseWrapper

-- We don't use this feature, so it's the same as simple 'testCase'.
testCaseSteps :: TestName -> ((String -> IO ()) -> Assertion) -> TestTree
testCaseSteps name f = testCase name (f $ const pass)

newtype TestCaseWrapper = TestCaseWrapper (IO String)

instance IsTest TestCaseWrapper where
  run _ (TestCaseWrapper assertion) _ = do
  -- The standard HUnit's performTestCase catches (almost) all exceptions.
  --
  -- This is bad for a few reasons:
  -- - it interferes with timeout handling
  -- - it makes exception reporting inconsistent across providers
  -- - it doesn't provide enough information for ingredients such as
  -- tasty-rerun
  --
  -- So we do it ourselves.
    hunitResult <- try @_ @HUnitFailure assertion
    return $
      case hunitResult of
        Right info -> testPassed info
        Left (HUnitFailure mbLoc reason) ->
          testFailed $ maybe id (mappend . (<> ":\n") . prettySrcLoc) mbLoc $
          formatFailureReason reason

  testOptions = return []
