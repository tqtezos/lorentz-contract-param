{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

module Michelson.Macro
  (
  -- * Macros types
    CadrStruct (..)
  , PairStruct (..)
  , Macro (..)
  , LetMacro (..)

  -- * Morley Parsed value types
  , ParsedValue

  -- * Morley Parsed instruction types
  , ParsedInstr
  , ParsedOp (..)
  , ParsedUTestAssert
  , ParsedUExtInstr

    -- * For utilities
  , expandContract
  , expandValue

    -- * For parsing
  , mapLeaves

    -- * Internals exported for tests
  , expand
  , expandList
  , expandPapair
  , expandUnpapair
  , expandCadr
  , expandSetCadr
  , expandMapCadr
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Generics (everywhere, mkT)
import qualified Data.Text as T
import Fmt (Buildable(build), genericF, (+|), (|+))
import qualified Text.PrettyPrint.Leijen.Text as PP (empty)

import Michelson.Printer (RenderDoc(..))
import Michelson.Untyped

-- | A programmer-defined macro
data LetMacro = LetMacro
  { lmName :: T.Text
  , lmSig :: StackFn
  , lmExpr :: [ParsedOp]
  } deriving (Eq, Show, Data, Generic)

instance Buildable LetMacro where
  build = genericF

data PairStruct
  = F (VarAnn, FieldAnn)
  | P PairStruct PairStruct
  deriving (Eq, Show, Data, Generic)

instance Buildable PairStruct where
  build = genericF

data CadrStruct
  = A
  | D
  deriving (Eq, Show, Data, Generic)

instance Buildable CadrStruct where
  build = genericF

-- | Unexpanded instructions produced directly by the @ops@ parser, which
-- contains primitive Michelson Instructions, inline-able macros and sequences
data ParsedOp
  = Prim ParsedInstr -- ^ Primitive Michelson instruction
  | Mac Macro        -- ^ Built-in Michelson macro defined by the specification
  | LMac LetMacro    -- ^ User-defined macro with instructions to be inlined
  | Seq [ParsedOp]   -- ^ A sequence of instructions
  deriving (Eq, Show, Data, Generic)

-- dummy value
instance RenderDoc ParsedOp where
  renderDoc _ = PP.empty

instance Buildable ParsedOp where
  build (Prim parseInstr) = "<Prim: "+|parseInstr|+">"
  build (Mac macro)       = "<Mac: "+|macro|+">"
  build (LMac letMacro)   = "<LMac: "+|letMacro|+">"
  build (Seq parsedOps)   = "<Seq: "+|parsedOps|+">"

-------------------------------------
-- Types produced by parser
-------------------------------------

type ParsedUTestAssert = TestAssert ParsedOp

type ParsedUExtInstr = ExtInstrAbstract ParsedOp

type ParsedInstr = InstrAbstract ParsedOp

type ParsedValue = Value' ParsedOp

-- | Built-in Michelson Macros defined by the specification
data Macro
  = CASE (NonEmpty [ParsedOp])
  | VIEW [ParsedOp]
  | VOID [ParsedOp]
  | CMP ParsedInstr VarAnn
  | IFX ParsedInstr [ParsedOp] [ParsedOp]
  | IFCMP ParsedInstr VarAnn [ParsedOp] [ParsedOp]
  | FAIL
  | PAPAIR PairStruct TypeAnn VarAnn
  | UNPAIR PairStruct
  | CADR [CadrStruct] VarAnn FieldAnn
  | SET_CADR [CadrStruct] VarAnn FieldAnn
  | MAP_CADR [CadrStruct] VarAnn FieldAnn [ParsedOp]
  | DIIP Integer [ParsedOp]
  | DUUP Integer VarAnn
  | ASSERT
  | ASSERTX ParsedInstr
  | ASSERT_CMP ParsedInstr
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME [ParsedOp] [ParsedOp]
  | IF_RIGHT [ParsedOp] [ParsedOp]
  deriving (Eq, Show, Data, Generic)

instance Buildable Macro where
  build (CASE parsedInstrs) = "<CASE: "+|toList parsedInstrs|+">"
  build (VIEW code) = "<VIEW: "+|code|+">"
  build (VOID code) = "<VOID: "+|code|+">"
  build (CMP parsedInstr carAnn) = "<CMP: "+|parsedInstr|+", "+|carAnn|+">"
  build (IFX parsedInstr parsedOps1 parsedOps2) = "<IFX: "+|parsedInstr|+", "+|parsedOps1|+", "+|parsedOps2|+">"
  build (IFCMP parsedInstr varAnn parsedOps1 parsedOps2) = "<IFCMP: "+|parsedInstr|+", "+|varAnn|+", "+|parsedOps1|+", "+|parsedOps2|+">"
  build FAIL = "FAIL"
  build (PAPAIR pairStruct typeAnn varAnn) = "<PAPAIR: "+|pairStruct|+", "+|typeAnn|+", "+|varAnn|+">"
  build (UNPAIR pairStruct) = "<UNPAIR: "+|pairStruct|+">"
  build (CADR cadrStructs varAnn fieldAnn) = "<CADR: "+|cadrStructs|+", "+|varAnn|+", "+|fieldAnn|+">"
  build (SET_CADR cadrStructs varAnn fieldAnn) = "<SET_CADR: "+|cadrStructs|+", "+|varAnn|+", "+|fieldAnn|+">"
  build (MAP_CADR cadrStructs varAnn fieldAnn parsedOps) = "<MAP_CADR: "+|cadrStructs|+", "+|varAnn|+", "+|fieldAnn|+", "+|parsedOps|+">"
  build (DIIP integer parsedOps) = "<DIIP: "+|integer|+", "+|parsedOps|+">"
  build (DUUP integer varAnn) = "<DUUP: "+|integer|+", "+|varAnn|+">"
  build ASSERT = "ASSERT"
  build (ASSERTX parsedInstr) = "<ASSERTX: "+|parsedInstr|+">"
  build (ASSERT_CMP parsedInstr) = "<ASSERT_CMP: "+|parsedInstr|+">"
  build ASSERT_NONE  = "ASSERT_NONE"
  build ASSERT_SOME  = "ASSERT_SOME"
  build ASSERT_LEFT  = "ASSERT_LEFT"
  build ASSERT_RIGHT = "ASSERT_RIGHT"
  build (IF_SOME parsedOps1 parsedOps2) = "<IF_SOME: "+|parsedOps1|+", "+|parsedOps2|+">"
  build (IF_RIGHT parsedOps1 parsedOps2) = "<IF_RIGHT: "+|parsedOps1|+", "+|parsedOps2|+">"

expandList :: [ParsedOp] -> [ExpandedOp]
expandList = fmap expand

-- | Expand all macros in parsed contract.
expandContract :: Contract' ParsedOp -> Contract
expandContract Contract {..} =
  Contract para stor (map (substituteTypes para stor) . expandList $ code)

substituteTypes :: Parameter -> Storage -> ExpandedOp -> ExpandedOp
substituteTypes param stor =
  everywhere $ mkT $
  \x -> case x of
    TypeParameter -> param
    TypeStorage -> stor
    t@(Type {}) -> t

-- Probably, some SYB can be used here
expandValue :: ParsedValue -> Value
expandValue = \case
  ValuePair l r -> ValuePair (expandValue l) (expandValue r)
  ValueLeft x -> ValueLeft (expandValue x)
  ValueRight x -> ValueRight (expandValue x)
  ValueSome x -> ValueSome (expandValue x)
  ValueNil -> ValueNil
  ValueSeq valueList -> ValueSeq (map expandValue valueList)
  ValueMap eltList -> ValueMap (map expandElt eltList)
  ValueLambda opList ->
    maybe ValueNil ValueLambda $
    nonEmpty (expandList $ toList opList)
  x -> fmap expand x

expandElt :: Elt ParsedOp -> Elt ExpandedOp
expandElt (Elt l r) = Elt (expandValue l) (expandValue r)

expand :: ParsedOp -> ExpandedOp
-- We handle this case specially, because it's essentially just PAIR.
-- It's needed because we have a hack in parser: we parse PAIR as PAPAIR.
-- We need to do something better eventually.
expand (Mac (PAPAIR (P (F a) (F b)) t v)) =
  PrimEx $ PAIR t v (snd a) (snd b)
expand (Mac m)  = SeqEx $ expandMacro m
expand (Prim i) = PrimEx $ expand <$> i
expand (Seq s)  = SeqEx $ expand <$> s
expand (LMac l)  = expandLetMac l
  where
    expandLetMac :: LetMacro -> ExpandedOp
    expandLetMac LetMacro {..} =
      PrimEx . EXT . FN lmName lmSig $
      expand <$> lmExpr

expandMacro :: Macro -> [ExpandedOp]
expandMacro = \case
  VIEW a             -> xp [ Mac $ UNPAIR $ P (F (noAnn,noAnn)) (F (noAnn,noAnn))
                           , Prim (DIP [Mac $ DUUP 2 noAnn]), Prim $ DUP noAnn
                           , Prim (DIP [Prim $ PAIR noAnn noAnn noAnn noAnn
                                       , Seq a, Prim $ SOME noAnn noAnn noAnn
                                       ])
                           , Prim $ PAIR noAnn noAnn noAnn noAnn
                           , Prim (DIP [Prim $ AMOUNT noAnn])
                           , Prim $ TRANSFER_TOKENS noAnn
                           , Prim $ NIL noAnn noAnn (Type TOperation noAnn)
                           , Prim $ SWAP , Prim $ CONS noAnn
                           , Prim $ PAIR noAnn noAnn noAnn noAnn
                           ]
  VOID a             -> xp [ Mac $ UNPAIR (P (F (noAnn,noAnn)) (F (noAnn,noAnn)))
                           , Prim SWAP, Prim $ DIP a, Prim SWAP, Prim $ EXEC noAnn
                           , Prim FAILWITH
                           ]
  CASE (x:|[])       -> expand <$> x
  CASE (i:|i':[])    -> xol $ IF_LEFT i i'
  CASE (i:|i':is)    -> xol $ IF_LEFT i [Mac $ CASE (i':|is)]
  CMP i v            -> [PrimEx (COMPARE v), xo i]
  IFX i bt bf        -> [xo i, PrimEx (IF (xp bt) (xp bf))]
  IFCMP i v bt bf    -> PrimEx <$> [COMPARE v, expand <$> i, IF (xp bt) (xp bf)]
  IF_SOME bt bf      -> [PrimEx (IF_NONE (xp bf) (xp bt))]
  IF_RIGHT bt bf     -> [PrimEx (IF_LEFT (xp bf) (xp bt))]
  FAIL               -> PrimEx <$> [UNIT noAnn noAnn, FAILWITH]
  ASSERT             -> xol $ IF [] [Mac FAIL]
  ASSERTX i          -> [expand $ Mac $ IFX i [] [Mac FAIL]]
  ASSERT_CMP i       -> [expand $ Mac $ IFCMP i noAnn [] [Mac FAIL]]
  ASSERT_NONE        -> xol $ IF_NONE [] [Mac FAIL]
  ASSERT_SOME        -> xol $ IF_NONE [Mac FAIL] []
  ASSERT_LEFT        -> xol $ IF_LEFT [] [Mac FAIL]
  ASSERT_RIGHT       -> xol $ IF_LEFT [Mac FAIL] []
  PAPAIR ps t v      -> expand <$> expandPapair ps t v
  UNPAIR ps          -> expand <$> expandUnpapair ps
  CADR c v f         -> expand <$> expandCadr c v f
  SET_CADR c v f     -> expand <$> expandSetCadr c v f
  MAP_CADR c v f ops -> expand <$> expandMapCadr c v f ops
  DIIP 1 ops         -> [PrimEx $ DIP (xp ops)]
  DIIP n ops         -> xol $  DIP [Mac $ DIIP (n - 1) ops]
  DUUP 1 v           -> [PrimEx $ DUP v]
  DUUP n v           -> [xo (DIP [Mac $ DUUP (n - 1) v]), PrimEx SWAP]
  where
    xol = one . xo
    xo = PrimEx . fmap expand
    xp = fmap expand

-- the correctness of type-annotation expansion is currently untested, as these
-- expansions are not explicitly documented in the Michelson Specification
expandPapair :: PairStruct -> TypeAnn -> VarAnn -> [ParsedOp]
expandPapair ps t v = case ps of
  P (F a) (F b) -> [Prim $ PAIR t v (snd a) (snd b)]
  P (F a) r     -> Prim <$> [ DIP [Mac $ PAPAIR r noAnn noAnn]
                            , PAIR t v (snd a) noAnn]
  P l     (F b) -> [ Mac $ PAPAIR l noAnn noAnn
                   , Prim $ PAIR t v noAnn (snd b)]
  P l     r     -> [ Mac $ PAPAIR l noAnn noAnn
                   , Prim $ DIP [Mac $ PAPAIR r noAnn noAnn]
                   , Prim $ PAIR t v noAnn noAnn]
  F _           -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.

expandUnpapair :: PairStruct -> [ParsedOp]
expandUnpapair = \case
  P (F (v,f)) (F (w,g)) -> Prim <$> [ DUP noAnn
                                    , CAR v f
                                    , DIP [Prim $ CDR w g]]
  P (F (v, f)) r        -> Prim <$> [ DUP noAnn
                                    , CAR v f
                                    , DIP [Prim $ CDR noAnn noAnn,
                                           Mac $ UNPAIR r]]
  P l     (F (v, f))    -> [ Prim (DUP noAnn)
                           , Prim (DIP [Prim $ CDR v f])
                           , Prim $ CAR noAnn noAnn
                           , Mac $ UNPAIR l]
  P l      r            -> [ Mac unpairOne
                           , Prim $ DIP [Mac $ UNPAIR r]
                           , Mac $ UNPAIR l]
  F _                   -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.
  where
    unpairOne = UNPAIR (P fn fn)
    fn = F (noAnn, noAnn)

expandCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandCadr cs v f = case cs of
  []    -> []
  [A]  -> [Prim $ CAR v f]
  [D]  -> [Prim $ CDR v f]
  A:css -> [Prim $ CAR noAnn noAnn, Mac $ CADR css v f]
  D:css -> [Prim $ CDR noAnn noAnn, Mac $ CADR css v f]

expandSetCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp]
expandSetCadr cs v f = Prim <$> case cs of
  []   -> []
  [A] -> [DUP noAnn, CAR noAnn f, DROP,
           -- ↑ These operations just check that the left element of pair has %f
           CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  [D] -> [DUP noAnn, CDR noAnn f, DROP,
           -- ↑ These operations just check that the right element of pair has %f
           CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  A:css -> [DUP noAnn, DIP [Prim carN, Mac $ SET_CADR css noAnn f], cdrN, SWAP, pairN]
  D:css -> [DUP noAnn, DIP [Prim cdrN, Mac $ SET_CADR css noAnn f], carN, pairN]
  where
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

expandMapCadr :: [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp] -> [ParsedOp]
expandMapCadr cs v f ops = case cs of
  []    -> []
  [A]  -> Prim <$> [DUP noAnn, cdrN, DIP [Prim $ CAR noAnn f, Seq ops], SWAP, pairN]
  [D]  -> concat [Prim <$> [DUP noAnn, CDR noAnn f], [Seq ops], Prim <$> [SWAP, carN, pairN]]
  A:css -> Prim <$> [DUP noAnn, DIP [Prim $ carN, Mac $ MAP_CADR css noAnn f ops], cdrN, SWAP, pairN]
  D:css -> Prim <$> [DUP noAnn, DIP [Prim $ cdrN, Mac $ MAP_CADR css noAnn f ops], carN, pairN]
  where
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

mapLeaves :: [(VarAnn, FieldAnn)] -> PairStruct -> PairStruct
mapLeaves fs p = evalState (leavesST p) fs

leavesST :: PairStruct -> State [(VarAnn, FieldAnn)] PairStruct
leavesST (P l r) = do
  l' <- leavesST l
  r' <- leavesST r
  return $ P l' r'
leavesST (F _) = do
  f <- state getLeaf
  return $ F f
  where
    getLeaf (a:as) = (a, as)
    getLeaf _      = ((noAnn, noAnn), [])

deriveJSON defaultOptions ''ParsedOp
deriveJSON defaultOptions ''LetMacro
deriveJSON defaultOptions ''PairStruct
deriveJSON defaultOptions ''CadrStruct
deriveJSON defaultOptions ''Macro
