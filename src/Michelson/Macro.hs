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
  , expandMacro
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
import Fmt (Buildable(build), genericF, (+|), (+||), (|+), (||+))
import qualified Text.PrettyPrint.Leijen.Text as PP (empty)

import Michelson.Printer (RenderDoc(..))
import Michelson.Untyped
import Util.Generic
import Michelson.ErrorPos

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
  = Prim ParsedInstr SrcPos -- ^ Primitive Michelson instruction
  | Mac Macro        SrcPos -- ^ Built-in Michelson macro defined by the specification
  | LMac LetMacro    SrcPos -- ^ User-defined macro with instructions to be inlined
  | Seq [ParsedOp]   SrcPos -- ^ A sequence of instructions
  deriving (Eq, Show, Data, Generic)

-- dummy value
instance RenderDoc ParsedOp where
  renderDoc _ = PP.empty

instance Buildable ParsedOp where
  build (Prim parseInstr _) = "<Prim: "+|parseInstr|+">"
  build (Mac macro _)       = "<Mac: "+|macro|+">"
  build (LMac letMacro _)   = "<LMac: "+|letMacro|+">"
  build (Seq parsedOps _)     = "<Seq: "+|parsedOps|+">"

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
  | TAG Natural (NonEmpty Type)
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
  build (TAG idx ty) = "<TAG: #"+||idx||+" from "+|toList ty|+""
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
expandList = fmap (expand [])

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
  x -> fmap (expand []) x

expandElt :: Elt ParsedOp -> Elt ExpandedOp
expandElt (Elt l r) = Elt (expandValue l) (expandValue r)

expand :: LetCallStack -> ParsedOp -> ExpandedOp
-- We handle this case specially, because it's essentially just PAIR.
-- It's needed because we have a hack in parser: we parse PAIR as PAPAIR.
-- We need to do something better eventually.
expand cs (Mac (PAPAIR (P (F a) (F b)) t v) pos) =
  WithSrcEx (InstrCallStack cs pos) $ PrimEx (PAIR t v (snd a) (snd b))
expand cs (Mac m pos)  = let ics = InstrCallStack cs pos in
  WithSrcEx ics $ SeqEx $ expandMacro ics m
expand cs (Prim i pos) = WithSrcEx (InstrCallStack cs pos) $ PrimEx $ expand cs <$> i
expand cs (Seq s pos)  = WithSrcEx (InstrCallStack cs pos) $ SeqEx $ expand cs <$> s
expand cs (LMac l pos) = expandLetMac l
  where
    expandLetMac :: LetMacro -> ExpandedOp
    expandLetMac LetMacro {..} =
      let newCS = LetName lmName : cs in
      let ics = InstrCallStack newCS pos in
      WithSrcEx ics $ PrimEx . EXT . FN lmName lmSig $ expand newCS <$> lmExpr

expandMacro :: InstrCallStack -> Macro -> [ExpandedOp]
expandMacro p@InstrCallStack{icsCallStack=cs,icsSrcPos=macroPos} = \case
  VIEW a             -> [ expand cs $ mac $ UNPAIR $ P (F (noAnn,noAnn)) (F (noAnn,noAnn))
                        , primEx (DIP [expand cs $ mac $ DUUP 2 noAnn])
                        , primEx $ DUP noAnn
                        , primEx $ DIP $ concat [
                              [primEx $ PAIR noAnn noAnn noAnn noAnn]
                            , expand cs <$> a
                            , [primEx $ SOME noAnn noAnn noAnn]
                            ]
                        , primEx $ PAIR noAnn noAnn noAnn noAnn
                        , primEx (DIP [primEx $ AMOUNT noAnn])
                        , primEx $ TRANSFER_TOKENS noAnn
                        , primEx $ NIL noAnn noAnn (Type TOperation noAnn)
                        , primEx $ SWAP
                        , primEx $ CONS noAnn
                        , primEx $ PAIR noAnn noAnn noAnn noAnn
                        ]
  VOID a             -> [ expand cs $ mac $ UNPAIR (P (F (noAnn,noAnn)) (F (noAnn,noAnn)))
                        , primEx SWAP
                        , primEx $ DIP $ expand cs <$> a
                        , primEx SWAP
                        , primEx $ EXEC noAnn
                        , primEx FAILWITH
                        ]
  CASE is            -> mkGenericTree (\_ l r -> one . PrimEx $ IF_LEFT l r)
                                      (map (expand cs) <$> is)
  TAG idx uty        -> expandTag (fromIntegral idx) uty
  CMP i v            -> [primEx (COMPARE v), xo i]
  IFX i bt bf        -> [xo i, primEx $ IF (xp bt) (xp bf)]
  IFCMP i v bt bf    -> primEx <$> [COMPARE v, expand cs <$> i, IF (xp bt) (xp bf)]
  IF_SOME bt bf      -> [primEx (IF_NONE (xp bf) (xp bt))]
  IF_RIGHT bt bf     -> [primEx (IF_LEFT (xp bf) (xp bt))]
  FAIL               -> primEx <$> [UNIT noAnn noAnn, FAILWITH]
  ASSERT             -> xol $ IF [] [mac FAIL]
  ASSERTX i          -> [expand cs $ mac $ IFX i [] [mac FAIL]]
  ASSERT_CMP i       -> [expand cs $ mac $ IFCMP i noAnn [] [mac FAIL]]
  ASSERT_NONE        -> xol $ IF_NONE [] [mac FAIL]
  ASSERT_SOME        -> xol $ IF_NONE [mac FAIL] []
  ASSERT_LEFT        -> xol $ IF_LEFT [] [mac FAIL]
  ASSERT_RIGHT       -> xol $ IF_LEFT [mac FAIL] []
  PAPAIR ps t v      -> expandPapair p ps t v
  UNPAIR ps          -> expandUnpapair p ps
  CADR c v f         -> expandCadr p c v f
  SET_CADR c v f     -> expandSetCadr p c v f
  MAP_CADR c v f ops -> expandMapCadr p c v f ops
  DIIP 1 ops         -> [primEx $ DIP (xp ops)]
  DIIP n ops         -> xol $ DIP [mac $ DIIP (n - 1) ops]
  DUUP 1 v           -> [primEx $ DUP v]
  DUUP n v           -> [xo (DIP [mac $ DUUP (n - 1) v]), primEx SWAP]
  where
    mac = flip Mac macroPos
    primEx = PrimEx
    xol = one . xo
    xo = primEx . fmap (expand cs)
    xp = fmap (expand cs)

-- the correctness of type-annotation expansion is currently untested, as these
-- expansions are not explicitly documented in the Michelson Specification
expandPapair :: InstrCallStack -> PairStruct -> TypeAnn -> VarAnn -> [ExpandedOp]
expandPapair ics ps t v = case ps of
  P (F a) (F b) -> [primEx $ PAIR t v (snd a) (snd b)]
  P (F a) r     -> primEx <$> [ DIP $ expandMacro ics (PAPAIR r noAnn noAnn)
                              , PAIR t v (snd a) noAnn]
  P l     (F b) -> expandMacro ics (PAPAIR l noAnn noAnn) ++
                   [primEx $ PAIR t v noAnn (snd b)]
  P l     r     -> expandMacro ics (PAPAIR l noAnn noAnn) ++
                   [ primEx $ DIP $ expandMacro ics (PAPAIR r noAnn noAnn)
                   , primEx $ PAIR t v noAnn noAnn]
  F _           -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.
  where
    primEx = PrimEx

expandUnpapair :: InstrCallStack -> PairStruct -> [ExpandedOp]
expandUnpapair ics = \case
  P (F (v,f)) (F (w,g)) ->
    primEx <$> [ DUP noAnn
               , CAR v f
               , DIP [primEx $ CDR w g]
               ]
  P (F (v, f)) r ->
    primEx <$> [ DUP noAnn
               , CAR v f
               , DIP (primEx (CDR noAnn noAnn) : expandMacro ics (UNPAIR r))
               ]
  P l     (F (v, f))    ->
    map primEx [ DUP noAnn
               , DIP [primEx $ CDR v f]
               , CAR noAnn noAnn
               ] ++
               expandMacro ics (UNPAIR l)
  P l      r ->
    expandMacro ics unpairOne ++
    [primEx $ DIP $ expandMacro ics $ UNPAIR r] ++
    expandMacro ics (UNPAIR l)
  F _                   -> [] -- Do nothing in this case.
  -- It's impossible from the structure of PairStruct and considered cases above,
  -- but if it accidentally happened let's just do nothing.
  where
    primEx = PrimEx
    unpairOne = UNPAIR (P fn fn)
    fn = F (noAnn, noAnn)

expandCadr :: InstrCallStack -> [CadrStruct] -> VarAnn -> FieldAnn -> [ExpandedOp]
expandCadr ics cs v f = case cs of
  []    -> []
  [A]  -> [primEx $ CAR v f]
  [D]  -> [primEx $ CDR v f]
  A:css -> primEx (CAR noAnn noAnn) : expandMacro ics (CADR css v f)
  D:css -> primEx (CDR noAnn noAnn) : expandMacro ics (CADR css v f)
  where
    primEx = PrimEx

expandSetCadr :: InstrCallStack -> [CadrStruct] -> VarAnn -> FieldAnn -> [ExpandedOp]
expandSetCadr ics cs v f = primEx <$> case cs of
  []    -> []
  [A]   -> [DUP noAnn, CAR noAnn f, DROP,
           -- ↑ These operations just check that the left element of pair has %f
           CDR (ann "%%") noAnn, SWAP, PAIR noAnn v f (ann "@")]
  [D]   -> [DUP noAnn, CDR noAnn f, DROP,
           -- ↑ These operations just check that the right element of pair has %f
           CAR (ann "%%") noAnn, PAIR noAnn v (ann "@") f]
  A:css -> [DUP noAnn, DIP (primEx carN : expandMacro ics (SET_CADR css noAnn f)), cdrN, SWAP, pairN]
  D:css -> [DUP noAnn, DIP (primEx cdrN : expandMacro ics (SET_CADR css noAnn f)), carN, pairN]
  where
    primEx = PrimEx
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

expandMapCadr :: InstrCallStack -> [CadrStruct] -> VarAnn -> FieldAnn -> [ParsedOp] -> [ExpandedOp]
expandMapCadr ics@InstrCallStack{icsCallStack=cls} cs v f ops = case cs of
  []    -> []
  [A]   -> primEx <$> [DUP noAnn, cdrN, DIP [primEx $ CAR noAnn f, SeqEx (expand cls <$> ops)], SWAP, pairN]
  [D]   -> concat [primEx <$> [DUP noAnn, CDR noAnn f], [SeqEx (expand cls <$> ops)], primEx <$> [SWAP, carN, pairN]]
  A:css -> primEx <$> [DUP noAnn, DIP (primEx carN : expandMacro ics (MAP_CADR css noAnn f ops)), cdrN, SWAP, pairN]
  D:css -> primEx <$> [DUP noAnn, DIP (primEx cdrN : expandMacro ics (MAP_CADR css noAnn f ops)), carN, pairN]
  where
    primEx = PrimEx
    carN = CAR noAnn noAnn
    cdrN = CDR noAnn noAnn
    pairN = PAIR noAnn v noAnn noAnn

expandTag :: Int -> NonEmpty Type -> [ExpandedOp]
expandTag idx unionTy =
  reverse . fst $ mkGenericTree merge (([], ) <$> unionTy)
  where
    merge i (li, lt) (ri, rt) =
      let ty = Type (TOr noAnn noAnn lt rt) noAnn
      in if idx < i
          then (PrimEx (LEFT noAnn noAnn noAnn noAnn rt) : li, ty)
          else (PrimEx (RIGHT noAnn noAnn noAnn noAnn lt) : ri, ty)

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
