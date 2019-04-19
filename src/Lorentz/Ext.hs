module Lorentz.Ext
  (
  -- * PRINT Morley command
    stackRef
  , printComment
  -- * TEST_ASSERT Morley command
  , testAssert
  ) where

import Data.Singletons (SingI)
import Lorentz.Type
import Michelson.Typed.Instr
import Util.Peano

stackRef
  :: forall (gn :: Nat) st n.
      (n ~ ToPeano gn, SingI n, KnownPeano n, RequireLongerThan st n)
  => PrintComment st
stackRef = PrintComment . one . Right $ mkStackRef @gn

printComment :: PrintComment s -> s :+> s
printComment = Ext . PRINT

testAssert
  :: Typeable out
  => Text -> PrintComment inp -> inp :+> TBool & out -> inp :+> inp
testAssert msg comment instr = (Ext . TEST_ASSERT) $ TestAssert msg comment instr

_sample1 :: s ~ (a & s') => s :+> s
_sample1 = printComment $ "Head is " <> stackRef @0
