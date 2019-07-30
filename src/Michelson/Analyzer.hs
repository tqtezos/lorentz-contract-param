{-# LANGUAGE DerivingStrategies #-}

-- | Static analysis of Michelson code.

module Michelson.Analyzer
  ( AnalyzerRes (..)
  , analyze
  ) where

import qualified Data.HashMap.Strict as HM
import Fmt (Buildable(..), Builder, blockMapF', hexF, nameF, (+|))

import Michelson.Text (MText)
import Michelson.Typed

data AnalyzerRes = AnalyzerRes
  { arConstStrings :: !(HashMap MText Word)
  -- ^ All string constants and number of their occurrences.
  , arConstBytes :: !(HashMap ByteString Word)
  -- ^ All bytes constants and number of their occurrences.
  } deriving stock (Show, Eq)

instance Buildable AnalyzerRes where
  build (AnalyzerRes (toPairs -> constStrings) (toPairs -> constBytes)) =
    nameF "String constants"
      (buildStrings $ sortByCount constStrings)
    +|
    nameF "Bytes constants"
      (buildBytes $ sortByCount constBytes)
    +|
    longest "strings" constStrings buildStrings
    +|
    longest "bytes" constBytes buildBytes
    where
      inQuotes toBuilder x = "\"" <> toBuilder x <> "\""

      buildStrings = blockMapF' (inQuotes build) build
      buildBytes = blockMapF' (mappend "0x" . hexF) build

      sortByCount :: [(k, Word)] -> [(k, Word)]
      sortByCount = sortWith snd

      sortByLength :: Container k => [(k, Word)] -> [(k, Word)]
      sortByLength = sortWith (Down . length . fst)

      longest ::
        Container x =>
        Builder -> [(x, Word)] -> ([(x, Word)] -> Builder) -> Builder
      longest name items builder
        | length items > 6 =
          nameF ("Longest " <> name) $ builder $ take 4 $ sortByLength items
        | otherwise = mempty

instance Semigroup AnalyzerRes where
  ar1 <> ar2 = AnalyzerRes
    { arConstStrings = arConstStrings ar1 <+> arConstStrings ar2
    , arConstBytes = arConstBytes ar1 <+> arConstBytes ar2
    }
    where
      m1 <+> m2 = HM.unionWith (+) m1 m2

instance Monoid AnalyzerRes where
  mempty = AnalyzerRes
    { arConstStrings = mempty
    , arConstBytes = mempty
    }

-- | Statically analyze an instruction. Typed representation is used
-- because it's easier to analyze. It means that we can't analyze
-- ill-typed contracts, but hopefully it's not a serious limitation.
analyze :: Instr inp out -> AnalyzerRes
analyze = dfsInstr True $ \case
  PUSH v -> onPush v
  _ -> mempty
  where
    onPush :: Value t -> AnalyzerRes
    onPush = \case
      VC (CvString str) -> mempty { arConstStrings = one (str, 1) }
      VC (CvBytes str) -> mempty { arConstBytes = one (str, 1) }
      _ -> mempty

-- | Traverse a typed instruction in depth-first order.
-- '<>' is used to concatenate intermediate results.
-- First argument specifies whether this function should go into
-- values (lambdas) and constant contracts (which can be passed
-- to @CREATE_CONTRACT@).
-- It does not consider extra instructions (not present in Michelson).
dfsInstr ::
     forall x inp out. Monoid x
  => Bool
  -> (forall i o. Instr i o -> x)
  -> Instr inp out
  -> x
dfsInstr goToValues step i = step i <>
  case i of
    Seq i1 i2 -> recursion2 i1 i2
    Nested i1 -> recursion1 i1
    PUSH (VLam i1) | goToValues -> recursion1 i1
    LAMBDA (VLam i1) | goToValues -> recursion1 i1
    IF_NONE i1 i2 -> recursion2 i1 i2
    IF_LEFT i1 i2 -> recursion2 i1 i2
    IF_CONS i1 i2 -> recursion2 i1 i2
    IF i1 i2 -> recursion2 i1 i2
    MAP i1 -> recursion1 i1
    ITER i1 -> recursion1 i1
    LOOP i1 -> recursion1 i1
    LOOP_LEFT i1 -> recursion1 i1
    DIP i1 -> recursion1 i1
    CREATE_CONTRACT i1 | goToValues -> recursion1 i1
    _ -> mempty
  where
    recursion1 :: forall i o. Instr i o -> x
    recursion1 = dfsInstr goToValues step

    recursion2 :: forall i1 o1 i2 o2. Instr i1 o1 -> Instr i2 o2 -> x
    recursion2 i1 i2 = recursion1 i1 <> recursion1 i2
