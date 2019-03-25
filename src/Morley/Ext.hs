module Morley.Ext
  ( interpretMorleyUntyped
  , interpretMorley
  , typeCheckMorleyContract
  , typeCheckHandler
  , interpretHandler
  ) where

import Control.Monad.Except (liftEither, throwError)
import Data.Default (def)
import Data.Map.Lazy (Map, insert, lookup)
import qualified Data.Map.Lazy as Map
import Data.Singletons (Sing)
import qualified Data.Text as T
import Data.Typeable ((:~:)(..))
import Data.Vinyl (Rec(..))

import Michelson.Interpret
  (ContractEnv, ContractReturn, EvalOp, InterpretUntypedError, InterpretUntypedResult,
  InterpreterEnv(..), InterpreterState(..), MichelsonFailed(..), SomeItStack(..), interpret,
  interpretUntyped, runInstrNoGas)
import Michelson.TypeCheck
import Michelson.TypeCheck.Helpers (convergeHST, eqT')
import Michelson.TypeCheck.Types (HST)
import Michelson.Typed (Val, converge, extractNotes, mkUType)
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..), InstrAbstract(..))
import Morley.Types

interpretMorleyUntyped
  :: Contract Op
  -> Value Op
  -> Value Op
  -> ContractEnv
  -> Either (InterpretUntypedError MorleyLogs) (InterpretUntypedResult MorleyLogs)
interpretMorleyUntyped c v1 v2 cenv =
  interpretUntyped typeCheckHandler c v1 v2 (InterpreterEnv cenv interpretHandler) def

interpretMorley
  :: (Typeable cp, Typeable st)
  => T.Contract cp st
  -> Val T.Instr cp
  -> Val T.Instr st
  -> ContractEnv
  -> ContractReturn MorleyLogs st
interpretMorley c param initSt env =
  interpret c param initSt (InterpreterEnv env interpretHandler) def

typeCheckMorleyContract :: Contract Instr -> Either TCError SomeContract
typeCheckMorleyContract = typeCheckContract typeCheckHandler

typeCheckHandler :: UExtInstr -> TcExtFrames -> SomeHST -> TypeCheckT (TcExtFrames, Maybe ExtInstr)
typeCheckHandler ext nfs hst@(SomeHST hs) =
  case ext of
    STACKTYPE s -> fitError $ const (nfs, Nothing) <$> checkStackType noBoundVars s hs
    FN t sf     -> fitError $ (, Nothing) <$> checkFn t sf hst nfs
    FN_END      -> fitError $ const (safeTail nfs, Nothing) <$> checkFnEnd hst nfs
    UPRINT pc   -> verifyPrint pc $> (nfs, Just $ PRINT pc)
    UTEST_ASSERT UTestAssert{..} -> do
      verifyPrint tassComment
      si <- typeCheckList (unOp <$> tassInstrs) hst
      case si of
        SiFail -> thErr "TEST_ASSERT has to return Bool, but it's failed"
        instr ::: (_ :: HST inp, ((_ :: (Sing b, T.Notes b, VarAnn)) ::& (_ :: HST out1))) -> do
          Refl <- liftEither $
                    first (const $ TCOtherError "TEST_ASSERT has to return Bool, but returned something else") $
                      eqT' @b @('T.Tc 'CBool)
          pure (nfs, Just $ TEST_ASSERT $ TestAssert tassName tassComment instr)
        _ -> thErr "TEST_ASSERT has to return Bool, but the stack is empty"
  where
    lhs = lengthHST hs
    thErr = throwError . TCOtherError

    verifyPrint :: PrintComment -> TypeCheckT ()
    verifyPrint (PrintComment pc) = do
      let checkStRef (Left _) = pure ()
          checkStRef (Right (StackRef (fromIntegral -> i)))
            | i < 0     = thErr $ "Stack reference is negative " <> show i
            | i >= lhs  = thErr $ "Stack reference is out of the stack: " <> show i <> " >= " <> show lhs
            | otherwise = pure ()
      traverse_ checkStRef pc

    safeTail :: [a] -> [a]
    safeTail (_:as) = as
    safeTail [] = []

    fitError = liftEither . first (TCFailedOnInstr (EXT ext) hst . flip uextErrorText hs)

interpretHandler :: (ExtInstr, SomeItStack) -> EvalOp MorleyLogs ()
interpretHandler (PRINT (PrintComment pc), SomeItStack st) = do
  let getEl (Left l) = l
      getEl (Right (StackRef i)) =
        fromMaybe (error "StackRef " <> show i <> " has to exist in the stack after typechecking, but it doesn't") $
        rat st (fromIntegral i)
  modify (\s -> s {isExtState = MorleyLogs $ mconcat (map getEl pc) : unMorleyLogs (isExtState s)})
interpretHandler (TEST_ASSERT (TestAssert nm pc (instr :: T.Instr inp1 ('T.Tc 'T.CBool ': out1) )),
            SomeItStack (st :: Rec (Val T.Instr) inp2)) = do
  Refl <- liftEither $ first (error "TEST_ASSERT input stack doesn't match") $ eqT' @inp1 @inp2
  runInstrNoGas instr st >>= \case
    (T.VC (T.CvBool False) :& RNil) -> do
      interpretHandler (PRINT pc, SomeItStack st)
      throwError $ MichelsonFailedOther $ "TEST_ASSERT " <> nm <> " failed"
    _  -> pass

-- | Various type errors possible when checking a @NopInstr@ with the
-- @nopHandler@
data UExtError =
    LengthMismatch StackTypePattern Int
  | VarError Text StackFn
  | TypeMismatch StackTypePattern Int Text
  | TyVarMismatch Var Type StackTypePattern Int Text
  | FnEndMismatch (Maybe (UExtInstr, SomeHST))
  | StkRestMismatch StackTypePattern SomeHST SomeHST Text
  | UnexpectedUExt UExtInstr

-- | Print error messages
uextErrorText :: UExtError -> HST xs -> Text
uextErrorText (LengthMismatch stk n) it = T.concat
  ["Unexpected length of stack: pattern ", show stk, " has length ", show n
  , ", but actual stack is", show it
  ]
uextErrorText (VarError t sf) _ = "In definition of " <> show t <> ": VarError " <> show sf
uextErrorText (TypeMismatch s n e) it = T.concat
  [ "TypeMismatch: Pattern ", show s, " failed on stack ", show it
  , "at index ", show n, " with \"", e, "\""
  ]
uextErrorText (TyVarMismatch v t s n e) it = T.concat
  [ "TyVarMismatch: Variable ", show v, " is bound to type ", show t
  , "but pattern ", show s, " failed on stack ", show it, "at index ", show n
  , " with \"", e, "\""
  ]
uextErrorText (FnEndMismatch n) it = "FnEndMismatch: " <> show n <> " on " <> show it
uextErrorText (UnexpectedUExt n) it = "UnexpectedUExt: " <> show n <> " on " <> show it
uextErrorText (StkRestMismatch s (SomeHST r) (SomeHST r') e) it = T.concat
  ["StkRestMismatch on stack ", show it
  , " in pattern " , show s
  , " against stacks ", show r, " and ", show r'
  , " with error: ", e
  ]

-- | Check that the optional "forall" variables are consistent if present
checkVars :: Text -> StackFn -> Either UExtError ()
checkVars t sf = case quantifiedVars sf of
  Just qs
    | varSet (inPattern sf) /= qs -> Left $ VarError t sf
  _ -> pure ()

-- | Checks the pattern in @FN@ and pushes a @ExtFrame@ onto the state
checkFn :: Text -> StackFn -> SomeHST -> TcExtFrames -> Either UExtError TcExtFrames
checkFn t sf si@(SomeHST it) nfs = do
  checkVars t sf
  second (const $ (FN t sf, si) : nfs) (checkStackType noBoundVars (inPattern sf) it)

-- |  Pops a @ExtFrame@ off the state and checks an @FN_END@ based on it
checkFnEnd :: SomeHST -> TcExtFrames -> Either UExtError BoundVars
checkFnEnd (SomeHST it') (nf@(nop, SomeHST it):_) = case nop of
  FN t sf -> do
    checkVars t sf
    m <- checkStackType noBoundVars (inPattern sf) it
    checkStackType m (outPattern sf) it'
  _ -> Left $ FnEndMismatch (Just nf)
checkFnEnd _ _ = Left $ FnEndMismatch Nothing

data BoundVars = BoundVars (Map Var Type) (Maybe SomeHST)

noBoundVars :: BoundVars
noBoundVars = BoundVars Map.empty Nothing

-- | Check that a @StackTypePattern@ matches the type of the current stack
checkStackType :: Typeable xs => BoundVars -> StackTypePattern -> HST xs
               -> Either UExtError BoundVars
checkStackType (BoundVars vars boundStkRest) s it = go vars 0 s it
  where
    go :: Typeable xs => Map Var Type -> Int -> StackTypePattern -> HST xs
       -> Either UExtError BoundVars
    go m _ StkRest sr = case boundStkRest of
      Nothing -> pure $ BoundVars m (Just $ SomeHST sr)
      Just si@(SomeHST sr') ->
        bimap (StkRestMismatch s (SomeHST sr) si)
              (const $ BoundVars m (Just si))
              (eqHST sr sr')
    go m _ StkEmpty SNil = pure $ BoundVars m Nothing
    go _ n StkEmpty _    = Left $ LengthMismatch s n
    go _ n _ SNil        = Left $ LengthMismatch s n
    go m n (StkCons (TyCon t) ts) ((xt, xann, _) ::& xs) = do
      tann <- first (TypeMismatch s n) (extractNotes t xt)
      void $ first (TypeMismatch s n) (converge tann xann)
      go m (n + 1) ts xs
    go m n (StkCons (VarID v) ts) ((xt, xann, _) ::& xs) =
      case lookup v m of
        Nothing -> let t = mkUType xt xann in go (insert v t m) (n + 1) ts xs
        Just t -> do
          tann <- first (TyVarMismatch v t s n) (extractNotes t xt)
          void $ first (TyVarMismatch v t s n) (converge tann xann)
          go m (n + 1) ts xs

eqHST :: (Typeable as, Typeable bs) => HST as -> HST bs -> Either Text (as :~: bs)
eqHST (it :: HST xs) (it' :: HST ys) = do
  Refl <- (eqT' @xs @ys)
  convergeHST it it'
  return Refl

lengthHST :: HST xs -> Int
lengthHST (_ ::& xs) = 1 + lengthHST xs
lengthHST SNil = 0

rat :: Rec (Val T.Instr) xs -> Int -> Maybe Text
rat (x :& _) 0 = Just $ show x
rat (_ :& xs) i = rat xs (i - 1)
rat RNil _ = Nothing
