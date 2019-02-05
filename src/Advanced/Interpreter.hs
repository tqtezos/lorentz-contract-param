{-# LANGUAGE TypeApplications #-}

-- | Module, containing function to interpret Michelson
-- instructions against given context and input stack.
module Advanced.Interpreter
  ( Operation (..)
  , run
  ) where

import Data.Vinyl (Rec(..))

import Advanced.Type (CT(..), T(..))
import Advanced.CValue (CVal (..), Address)
import Advanced.Arith (evalOp, Add)
import Advanced.Value (Val (..), Instr (..), (#))

-- | Data type, representing operation, list of which is returned
-- by Michelson contract (according to calling convention).
--
-- These operations are to be further executed against system state
-- after the contract execution.
data Operation where
  TransferTokens :: Show p => p -> Int64 -> Address -> Operation

deriving instance Show Operation

-- | Function to interpret Michelson instruction(s) against given stack.
run :: Instr Operation inp out -> Rec (Val Operation) inp -> Rec (Val Operation) out
run (Seq i1 i2) r = run i2 (run i1 r)
run Nop r = r
run DROP (_ :& r) = r
run DUP (a :& r) = a :& a :& r
run SWAP (a :& b :& r) = b :& a :& r
run (PUSH v) r = v :& r
run SOME (a :& r) = VOption (Just a) :& r
run NONE r = VOption Nothing :& r
run UNIT r = VUnit :& r
run (IF_NONE bNone bJust) (VOption m :& r) = maybe (run bNone r) (run bJust . (:& r)) m
run PAIR (a :& b :& r) = VPair (a, b) :& r
run CAR (VPair (a, b) :& r) = a :& r
run CDR (VPair (a, b) :& r) = b :& r
-- More here
run (DIP i) (a :& r) = a :& run i r
-- More here
run ADD (VC l :& VC r :& rest) = VC (evalOp (Proxy @Add) l r) :& rest
-- More here
run TRANSFER_TOKENS (p :& VC (CvMutez mutez) :& VContract addr :& r) = VOp (TransferTokens p mutez addr) :& r


--------------------
-- Examples
--------------------

-- | @myInstr@ is an equivalent to Michelson code:
--
--    PUSH int 223;
--    SOME;
--    IF_NONE DUP SWAP;
--    ADD
myInstr =
  PUSH (VC $ CvInt 223) #
  SOME #
  IF_NONE DUP SWAP #
  ADD #
  PUSH (VC $ CvNat 12) #
  ADD

-- | @myInstr2@ can not be represented in Michelson
-- syntax as Michelson has no way to directly push value
-- of type "option int"
myInstr2 =
  PUSH (VOption $ Just $ VC $ CvInt 223) #
  IF_NONE DUP Nop

myInstrEvaluated :: Rec (Val Operation) '[ 'T_c 'T_int ]
myInstrEvaluated = run myInstr (VC (CvInt 90) :& RNil)
