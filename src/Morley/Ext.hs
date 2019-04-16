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
import Data.Typeable ((:~:)(..))
import Data.Vinyl (Rec(..))

import Michelson.Interpret
  (ContractEnv(..), ContractReturn, EvalOp, InterpretUntypedError, InterpretUntypedResult,
  InterpreterEnv(..), InterpreterState(..), MichelsonFailed(..), SomeItStack(..), interpret,
  interpretUntyped, runInstrNoGas)
import Michelson.TypeCheck
import Michelson.TypeCheck.Helpers (convergeHST, eqType, onLeft)
import Michelson.TypeCheck.Types (HST)
import Michelson.Typed (converge, extractNotes, mkUType)
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..))
import qualified Michelson.Untyped as U
import Morley.Types
import Util.Peano (LongerThan, Peano, Sing(SS, SZ))

interpretMorleyUntyped
  :: U.Contract
  -> U.Value
  -> U.Value
  -> ContractEnv
  -> Either (InterpretUntypedError MorleyLogs) (InterpretUntypedResult MorleyLogs)
interpretMorleyUntyped c v1 v2 cenv =
  interpretUntyped typeCheckHandler c v1 v2 (InterpreterEnv cenv interpretHandler) def

interpretMorley
  :: T.Contract cp st
  -> T.Value cp
  -> T.Value st
  -> ContractEnv
  -> ContractReturn MorleyLogs st
interpretMorley c param initSt env =
  interpret c param initSt (InterpreterEnv env interpretHandler) def

typeCheckMorleyContract :: TcOriginatedContracts -> U.Contract -> Either TCError SomeContract
typeCheckMorleyContract = typeCheckContract typeCheckHandler

typeCheckHandler
  :: forall s.
     (Typeable s)
  => ExpandedUExtInstr
  -> TcExtFrames
  -> HST s
  -> TypeCheckT (TcExtFrames, Maybe (ExtInstr s))
typeCheckHandler ext nfs hst =
  case ext of
    STACKTYPE s -> fitError $ (nfs, Nothing) <$ checkStackType noBoundVars s hst
    FN t sf     -> fitError $ (, Nothing) <$> checkFn t sf hst nfs
    FN_END      -> fitError $ (safeTail nfs, Nothing) <$ checkFnEnd hst nfs
    UPRINT pc   -> verifyPrint pc <&> \tpc -> (nfs, Just $ PRINT tpc)
    UTEST_ASSERT UTestAssert{..} -> do
      verifyPrint tassComment
      _ :/ si <- typeCheckList tassInstrs hst
      case si of
        AnyOutInstr _ -> throwError $ TCExtError (SomeHST hst) $ TestAssertError
                         "TEST_ASSERT has to return Bool, but it always fails"
        instr ::: (((_ :: (Sing b, T.Notes b, VarAnn)) ::& (_ :: HST out1))) -> do
          Refl <- liftEither $
                    first (const $ TCExtError (SomeHST hst) $
                           TestAssertError "TEST_ASSERT has to return Bool, but returned something else") $
                      eqType @b @('T.Tc 'CBool)
          tcom <- verifyPrint tassComment
          pure (nfs, Just $ TEST_ASSERT $ TestAssert tassName tcom instr)
        _ -> throwError $ TCExtError (SomeHST hst) $ TestAssertError "TEST_ASSERT has to return Bool, but the stack is empty"
  where
    verifyPrint :: UPrintComment -> TypeCheckT (PrintComment s)
    verifyPrint (UPrintComment pc) = do
      let checkStRef (Left txt) = pure (Left txt)
          checkStRef (Right (UStackRef i)) = Right <$> do
            liftEither $ createStackRef i hst
      PrintComment <$> traverse checkStRef pc

    safeTail :: [a] -> [a]
    safeTail (_:as) = as
    safeTail [] = []

    fitError = liftEither . first (TCExtError (SomeHST hst))

interpretHandler :: SomeItStack -> EvalOp MorleyLogs ()
interpretHandler (SomeItStack (PRINT (PrintComment pc)) st) = do
  let getEl (Left l) = l
      getEl (Right str) = withStackElem str st show
  modify (\s -> s {isExtState = MorleyLogs $ mconcat (map getEl pc) : unMorleyLogs (isExtState s)})

interpretHandler (SomeItStack (TEST_ASSERT (TestAssert nm pc (instr :: T.Instr inp1 ('T.Tc 'T.CBool ': out1) )))
            (st :: Rec T.Value inp2)) = do
  runInstrNoGas instr st >>= \case
    (T.VC (T.CvBool False) :& RNil) -> do
      interpretHandler (SomeItStack (PRINT pc) st)
      throwError $ MichelsonFailedOther $ "TEST_ASSERT " <> nm <> " failed"
    _  -> pass

-- | Check that the optional "forall" variables are consistent if present
checkVars :: Text -> StackFn -> Either ExtError ()
checkVars t sf = case quantifiedVars sf of
  Just qs
    | varSet (inPattern sf) /= qs -> Left $ VarError t sf
  _ -> pure ()

-- | Checks the pattern in @FN@ and pushes a @ExtFrame@ onto the state
checkFn
  :: Typeable s
  => Text -> StackFn -> HST s -> TcExtFrames -> Either ExtError TcExtFrames
checkFn t sf it nfs = do
  checkVars t sf
  second (const $ (FN t sf, SomeHST it) : nfs) (checkStackType noBoundVars (inPattern sf) it)

-- |  Pops a @ExtFrame@ off the state and checks an @FN_END@ based on it
checkFnEnd :: Typeable s => HST s -> TcExtFrames -> Either ExtError BoundVars
checkFnEnd it' (nf@(nop, SomeHST it):_) = case nop of
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
               -> Either ExtError BoundVars
checkStackType (BoundVars vars boundStkRest) s it = go vars 0 s it
  where
    go :: Typeable xs => Map Var Type -> Int -> StackTypePattern -> HST xs
       -> Either ExtError BoundVars
    go m _ StkRest sr = case boundStkRest of
      Nothing -> pure $ BoundVars m (Just $ SomeHST sr)
      Just si@(SomeHST sr') ->
        bimap (StkRestMismatch s (SomeHST sr) si)
              (const $ BoundVars m (Just si))
              (eqHST sr sr')
    go m _ StkEmpty SNil = pure $ BoundVars m Nothing
    go _ _ StkEmpty _    = Left $ LengthMismatch s
    go _ _ _ SNil        = Left $ LengthMismatch s
    go m n (StkCons (TyCon t) ts) ((xt, xann, _) ::& xs) = do
      tann <- first (TypeMismatch s n . ExtractionTypeMismatch) (extractNotes t xt)
      void $ first (TypeMismatch s n . AnnError) (converge tann xann)
      go m (n + 1) ts xs
    go m n (StkCons (VarID v) ts) ((xt, xann, _) ::& xs) =
      case lookup v m of
        Nothing -> let t = mkUType xt xann in go (insert v t m) (n + 1) ts xs
        Just t -> do
          tann <- first (TyVarMismatch v t s n . ExtractionTypeMismatch) (extractNotes t xt)
          void $ first (TyVarMismatch v t s n . AnnError) (converge tann xann)
          go m (n + 1) ts xs

eqHST :: (Typeable as, Typeable bs) => HST as -> HST bs -> Either TCTypeError (as :~: bs)
eqHST (it :: HST xs) (it' :: HST ys) = do
  Refl <- (eqType @xs @ys)
  void $ convergeHST it it' `onLeft` AnnError
  return Refl

lengthHST :: HST xs -> Natural
lengthHST (_ ::& xs) = 1 + lengthHST xs
lengthHST SNil = 0

-- | Create stack reference accessing element with a given index.
--
-- Fails when index is too large for the given stack.
createStackRef :: Typeable s => Natural -> HST s -> Either TCError (StackRef s)
createStackRef idx hst =
  case doCreate (hst, idx) of
    Just sr -> Right sr
    Nothing -> Left $
      TCExtError (SomeHST hst) $
      InvalidStackReference (UStackRef idx) (StackSize $ lengthHST hst)
  where
    doCreate :: forall s. (HST s, Natural) -> Maybe (StackRef s)
    doCreate = \case
      (SNil, _) -> Nothing
      ((_ ::& _), 0) -> Just (StackRef SZ)
      ((_ ::& st), i) -> do
        StackRef ns <- doCreate (st, i - 1)
        return $ StackRef (SS ns)

-- | Access given stack reference (in CPS style).
withStackElem
  :: forall st a.
     StackRef st
  -> Rec T.Value st
  -> (forall t. T.Value t -> a)
  -> a
withStackElem (StackRef sn) vals cont =
  loop (vals, sn)
  where
    loop
      :: forall s (n :: Peano). (LongerThan s n)
      => (Rec T.Value s, Sing n) -> a
    loop = \case
      (e :& _, SZ) -> cont e
      (_ :& es, SS n) -> loop (es, n)
