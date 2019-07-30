-- | Tests for analyzer.

module Test.Analyzer
  ( unit_Sample_analyze
  ) where

import Prelude hiding (EQ)

import qualified Data.HashMap.Strict as HM
import Test.HUnit (Assertion, (@?=))

import Michelson.Analyzer
import Michelson.Text
import qualified Michelson.Typed as T
import Michelson.Typed.Instr
import Michelson.Untyped (CT(..))

unit_Sample_analyze :: Assertion
unit_Sample_analyze = analyze sample @?= expectedRes
  where
    expectedRes = AnalyzerRes
      { arConstStrings = HM.fromList [(str1, 1), (str3, 3)]
      , arConstBytes = mempty
      }

str1 :: MText
str1 = [mt|aa|]

str3 :: MText
str3 = [mt|bb|]

sample :: T.Contract ('T.Tc 'CString) ('T.Tc 'CString)
sample =
  CAR `Seq` DUP `Seq`
  pushStr str3 `Seq`
  CONCAT `Seq`
  SIZE `Seq` INT `Seq` EQ `Seq`
  IF (LAMBDA (T.VLam $ pushStr str3 `Seq` CONCAT) `Seq` DROP)
     (DIP (pushStr str1) `Seq` DROP) `Seq`
  DIP (pushStr str3) `Seq` DROP `Seq`
  NIL `Seq` PAIR
  where
    pushStr :: forall s. MText -> Instr s ('T.Tc 'CString ': s)
    pushStr str = PUSH (T.VC $ T.CvString str)
