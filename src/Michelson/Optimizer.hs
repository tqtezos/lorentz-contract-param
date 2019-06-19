-- | Optimizer for typed instructions.

module Michelson.Optimizer
  ( optimize
  ) where

import Prelude hiding (EQ)

import Michelson.Typed.CValue
import Michelson.Typed.Instr
import Michelson.Typed.Value

-- | Optimize a typed instruction by replacing some sequences of
-- instructions with smaller equivalent sequences.
optimize :: Instr a b -> Instr a b
optimize = \case
  Seq SWAP (Seq SWAP i) -> optimize i
  Seq (PUSH _) (Seq DROP i) -> optimize i
  Seq (PUSH x) (Seq (DIP y) i) -> optimize $ Seq y $ Seq (PUSH x) i
  Seq LEFT (Seq (IF_LEFT bt _) i) -> optimize $ Seq bt i
  Seq RIGHT (Seq (IF_LEFT _ bf) i) -> optimize $ Seq bf i
  Seq CONS (Seq (IF_CONS bt _) i) -> optimize $ Seq bt i
  Seq NIL (Seq (IF_CONS _ bf) i) -> optimize $ Seq bf i
  Seq NONE (Seq (IF_NONE bt _) i) -> optimize $ Seq bt i
  Seq SOME (Seq (IF_NONE _ bf) i) -> optimize $ Seq bf i
  Seq (PUSH (VC (CvInt 0))) (Seq COMPARE (Seq EQ i)) ->
    Seq EQ $ optimize i
  Seq (PUSH (VC (CvNat 0))) (Seq COMPARE (Seq EQ i)) ->
    Seq INT $ Seq EQ $ optimize i
  Seq i1 i2 ->  Seq i1 $ optimize  i2
  i -> i
