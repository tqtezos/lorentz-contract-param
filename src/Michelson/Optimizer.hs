-- | Optimizer for typed instructions.
--
-- It's quite experimental and incomplete.
-- List of possible improvements:
-- 1. See comment to 'optimizeWithRule', it is not quite correct.
-- 2. 'pushDrop', 'dupDrop', 'unitDrop' rules are essentially the
-- same. It would be good to generalize them into one rule. The same
-- applies to 'pushDip'.
-- 3. Also we completely ignore 'Nested'.
-- 4. Some rules make break right-linear structure (e. g. pushDip) and
-- currently we use 'linearizeRight' to account for that.
-- It probably can be done more efficiently.

module Michelson.Optimizer
  ( optimize
  , optimizeWithRules
  , optimizeWithRule
  , linearizeRight
  , defaultRules
  ) where

import Prelude hiding (EQ)

import Michelson.Typed.CValue
import Michelson.Typed.Instr
import Michelson.Typed.Value

----------------------------------------------------------------------------
-- High level
----------------------------------------------------------------------------

-- | Optimize a typed instruction by replacing some sequences of
-- instructions with smaller equivalent sequences.
-- Applies default set of rewrite rules.
optimize :: Instr inp out -> Instr inp out
optimize = optimizeWithRules defaultRules

-- | Optimize a typed instruction using a custom set of rules.
optimizeWithRules :: (forall inp1 out1. Rules inp1 out1) -> Instr inp out -> Instr inp out
optimizeWithRules rules = optimizeWithRule (joinRules rules)

-- | Optimize a typed instruction using a specific rule.
--
-- It applies all rules to head (first few instructions) while they
-- successfully apply.  Then it applies them to instructions after the
-- first one and so on.
--
-- It's probably not the most efficient solution and it's not quite
-- correct, because if we have instructions `a; b; c` and optimize `b;
-- c` then we should return to optimizing sequence which starts with `a`.
optimizeWithRule :: Rule -> Instr a b -> Instr a b
optimizeWithRule rule =
  eraseNop . optimizeWithRule' . appendInstrRL Nop . linearizeRight
  where
    deepR :: Rule
    deepR = deepRule rule

    optimizeWithRule' :: Instr a b -> Instr a b
    optimizeWithRule' i =
      case deepR i of
        (optI, True) -> optimizeWithRule' optI
        -- Here `Seq j k` is `i`
        (Seq j k, False) -> Seq j (optimizeWithRule' k)
        _ -> i

-- | There are many ways to represent a sequence of more than 2 instructions.
-- E. g. for `i1; i2; i3` it can be @Seq i1 $ Seq i2 i3@ or @Seq i1 i2 $ i3@.
-- This function enforces a particular structure. Specifically, it makes each
-- 'Seq' have a single instruction (i. e. not 'Seq') in its first argument.
-- It makes it easier to implement optimizer.
-- This function also erases redundant 'Nop'.
--
-- TODO: currently it doesn't go deeper into instructions like 'IF'.
-- We should try reusing 'deepRule' for this purpose.
--
-- TODO: it also has bad worst-case performance, it should be carefully
-- rewritten.
linearizeRight :: Instr a b -> Instr a b
linearizeRight =
  \case
    Seq (Seq i1 i2) i3 -> linearizeRight $ Seq i1 $ linearizeRight $ Seq i2 i3
    Seq Nop i2 -> linearizeRight i2
    Seq i1 i2 -> Seq i1 $ linearizeRight i2
    i -> i

defaultRules :: Rules inp out
defaultRules =
  [ swapSwap
  , pushDrop
  , dupDrop
  , unitDrop
  , pushDip
  , leftIfLeft
  , rightIfLeft
  , consIfCons
  , nilIfCons
  , noneIfNone
  , someIfNone
  , compare0Int
  , compare0Nat
  ]

-- This is an internal function which appends second instr to the
-- first instr. The first instr is supposed to be right-linear and
-- this property is preserved.
-- We use it to append 'Nop' to each sequence of instructions. It allows
-- us to write rewrite rules for sequence of `a; b; c` regardless of
-- whether there is something else after `c`. We ensure there is
-- always 'Nop'.
--
-- TODO: we should go deeper into instructions like 'IF' and insert
-- 'Nop' there as well.
appendInstrRL :: Instr a b -> Instr b c -> Instr a c
appendInstrRL l r =
  case l of
    Seq i1 i2 -> Seq i1 (i2 `appendInstrRL` r)
    i -> Seq i r

-- This is an internal function which ensures 'Nop' is present in an
-- instruction only if it is exactly 'Nop' (not a sequence of
-- instructions).
--
-- TODO: currently it doesn't go deeper into instructions like 'IF'.
-- We should try reusing 'deepRule' for this purpose.
eraseNop :: Instr a b -> Instr a b
eraseNop = \case
  Seq Nop i -> eraseNop i
  Seq i Nop -> eraseNop i
  i -> i

----------------------------------------------------------------------------
-- Rewrite rules
----------------------------------------------------------------------------

-- Type of a single rewrite rule. It takes an instruction and tries to
-- optimize its head (first few instructions).  If optimization
-- succeeds, it returns optimized instruction and `True`, otherwise it
-- returns original instruction and `False`.
type Rule = forall inp out. Instr inp out -> (Instr inp out, Bool)

-- GHC doesn't yet support impredicative polymorphism, so we can't write `[Rule]`
type Rules inp out = [Instr inp out -> (Instr inp out, Bool)]

swapSwap :: Rule
swapSwap = \case
  Seq SWAP (Seq SWAP i) -> (i, True)
  i -> (i, False)

pushDrop :: Rule
pushDrop = \case
  Seq (PUSH _) (Seq DROP i) -> (i, True)
  i -> (i, False)

dupDrop :: Rule
dupDrop = \case
  Seq DUP (Seq DROP i) -> (i, True)
  i -> (i, False)

unitDrop :: Rule
unitDrop = \case
  Seq UNIT (Seq DROP i) -> (i, True)
  i -> (i, False)

pushDip :: Rule
pushDip = \case
  Seq (PUSH x) (Seq (DIP f) i) -> (linearizeRight $ Seq f (Seq (PUSH x) i), True)
  i -> (i, False)

leftIfLeft :: Rule
leftIfLeft = \case
  Seq LEFT (Seq (IF_LEFT bt _) i) -> (Seq bt i, True)
  i -> (i, False)

rightIfLeft :: Rule
rightIfLeft = \case
  Seq RIGHT (Seq (IF_LEFT _ bf) i) -> (Seq bf i, True)
  i -> (i, False)

consIfCons :: Rule
consIfCons = \case
  Seq CONS (Seq (IF_CONS bt _) i) -> (Seq bt i, True)
  i -> (i, False)

nilIfCons :: Rule
nilIfCons = \case
  Seq NIL (Seq (IF_CONS _ bf) i) -> (Seq bf i, True)
  i -> (i, False)

noneIfNone :: Rule
noneIfNone = \case
  Seq NONE (Seq (IF_NONE bt _) i) -> (Seq bt i, True)
  i -> (i, False)

someIfNone :: Rule
someIfNone = \case
  Seq SOME (Seq (IF_NONE _ bf) i) -> (Seq bf i, True)
  i -> (i, False)

compare0Int :: Rule
compare0Int = \case
  Seq (PUSH (VC (CvInt 0))) (Seq COMPARE (Seq EQ i)) -> (Seq EQ i, True)
  i -> (i, False)

compare0Nat :: Rule
compare0Nat = \case
  Seq (PUSH (VC (CvNat 0))) (Seq COMPARE (Seq EQ i)) -> (Seq INT $ Seq EQ i, True)
  i -> (i, False)

----------------------------------------------------------------------------
-- Generic functions working with rules
----------------------------------------------------------------------------

deepRule :: Rule -> Rule
deepRule rule i =
  case rule i of
    (optimizedI, True) -> (optimizedI, True)
    (_, False) -> goDeeper
  where
    opt :: forall a b. Instr a b -> (Instr a b, Bool)
    opt = deepRule rule

    goDeeper = case i of
      DIP j -> goDeeper1 DIP j
      LOOP j -> goDeeper1 LOOP j
      ITER j -> goDeeper1 ITER j
      LOOP_LEFT j -> goDeeper1 LOOP_LEFT j
      IF j k -> goDeeper2 IF j k
      IF_NONE j k -> goDeeper2 IF_NONE j k
      IF_LEFT j k -> goDeeper2 IF_LEFT j k
      IF_CONS j k -> goDeeper2 IF_CONS j k
      _ -> (i, False)

    goDeeper1 :: (Instr a b -> Instr c d) -> Instr a b -> (Instr c d, Bool)
    goDeeper1 constr j =
      case opt j of
        (optJ, True) -> (constr optJ, True)
        _ -> (constr j, False)

    goDeeper2 :: (Instr a b -> Instr c d -> Instr e f) -> Instr a b -> Instr c d -> (Instr e f, Bool)
    goDeeper2 constr j k =
      let (optJ, flag1) = opt j
      in let (optK, flag2) = opt k
      in (constr optJ optK, flag1 || flag2)

joinRules ::
     forall inp out.
     Rules inp out
  -> Instr inp out
  -> (Instr inp out, Bool)
joinRules [] i = (i, False)
joinRules (r:rs) i =
  case r i of
    (optI, True) -> (fst $ joinRules rs optI, True)
    (_, False) -> joinRules rs i
