module Lorentz.Ext
  (
  -- * PRINT Morley command
    stackRef
  , printComment
  -- * TEST_ASSERT Morley command
  , testAssert
  ) where

import Data.Singletons (SingI)

import Lorentz.Base
import Michelson.Typed.Haskell
import Michelson.Typed.Instr
import Util.Peano

stackRef
  :: forall (gn :: Nat) st n.
      (n ~ ToPeano gn, SingI n, KnownPeano n, RequireLongerThan st n)
  => PrintComment st
stackRef = PrintComment . one . Right $ mkStackRef @gn

printComment :: PrintComment (ToTs s) -> s :-> s
printComment = I . Ext . PRINT

testAssert
  :: Typeable (ToTs out)
  => Text -> PrintComment (ToTs inp) -> inp :-> Bool & out -> inp :-> inp
testAssert msg comment (I instr) =
  I . (Ext . TEST_ASSERT) $ TestAssert msg comment instr

_sample1 :: s ~ (a & s') => s :-> s
_sample1 = printComment $ "Head is " <> stackRef @0
