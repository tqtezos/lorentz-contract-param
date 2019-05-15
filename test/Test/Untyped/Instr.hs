-- | Tests for 'Michelson.Untyped.Instr'.

module Test.Untyped.Instr
  ( unit_Flattening
  ) where

import Test.HUnit (Assertion, (@?=))

import Michelson.Untyped.Instr

unit_Flattening :: Assertion
unit_Flattening = flattenExpandedOp sampleOp @?= expectedInstrs
  where
    sampleOp :: ExpandedOp
    sampleOp = SeqEx
      [ toSeq []
      , toSeq seq1
      , toPrim prim3
      ]

    toPrim :: ExpandedInstr -> ExpandedOp
    toPrim = PrimEx
    toSeq :: [ExpandedInstr] -> ExpandedOp
    toSeq = SeqEx . map PrimEx

    seq1 = [prim1, prim2]
    prim1 = DROP
    prim2 = SWAP
    prim3 = DIP
      [ toPrim prim1
      , toSeq seq1
      , toSeq seq1
      , toSeq []
      , SeqEx [toSeq seq1, toSeq seq1]
      ]

    expectedInstrs :: [ExpandedInstr]
    expectedInstrs =
      [ prim1
      , prim2
      , DIP $ map toPrim $
        prim1 : mconcat (replicate 4 seq1)
      ]
