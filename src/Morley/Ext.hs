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

interpretMorleyUntyped
  :: U.Contract
  -> U.Value
  -> U.Value
  -> ContractEnv
  -> Either (InterpretUntypedError MorleyLogs) (InterpretUntypedResult MorleyLogs)
interpretMorleyUntyped c v1 v2 cenv =
  interpretUntyped typeCheckHandler c v1 v2 (InterpreterEnv cenv interpretHandler) def

interpretMorley
  :: (Typeable cp, Typeable st)
  => T.Contract cp st
  -> T.Value cp
  -> T.Value st
  -> ContractEnv
  -> ContractReturn MorleyLogs st
interpretMorley c param initSt env =
  interpret c param initSt (InterpreterEnv env interpretHandler) def

typeCheckMorleyContract :: TcOriginatedContracts -> U.Contract -> Either TCError SomeContract
typeCheckMorleyContract = typeCheckContract typeCheckHandler

typeCheckHandler :: ExpandedUExtInstr -> TcExtFrames -> SomeHST -> TypeCheckT (TcExtFrames, Maybe ExtInstr)
typeCheckHandler ext nfs hst@(SomeHST hs) =
  case ext of
    STACKTYPE s -> fitError $ (nfs, Nothing) <$ checkStackType noBoundVars s hs
    FN t sf     -> fitError $ (, Nothing) <$> checkFn t sf hst nfs
    FN_END      -> fitError $ (safeTail nfs, Nothing) <$ checkFnEnd hst nfs
    UPRINT pc   -> verifyPrint pc $> (nfs, Just $ PRINT pc)
    UTEST_ASSERT UTestAssert{..} -> do
      verifyPrint tassComment
      si <- typeCheckList tassInstrs hs
      case si of
        SiFail -> throwError $ TCExtError hst $ TestAssertError "TEST_ASSERT has to return Bool, but it's failed"
        instr ::: (_ :: HST inp, ((_ :: (Sing b, T.Notes b, VarAnn)) ::& (_ :: HST out1))) -> do
          Refl <- liftEither $
                    first (const $ TCExtError hst $
                           TestAssertError "TEST_ASSERT has to return Bool, but returned something else") $
                      eqType @b @('T.Tc 'CBool)
          pure (nfs, Just $ TEST_ASSERT $ TestAssert tassName tassComment instr)
        _ -> throwError $ TCExtError hst $ TestAssertError "TEST_ASSERT has to return Bool, but the stack is empty"
  where
    lhs = lengthHST hs
    verifyPrint :: PrintComment -> TypeCheckT ()
    verifyPrint (PrintComment pc) = do
      let checkStRef (Left _) = pure ()
          checkStRef (Right ref@(StackRef i))
            | i >= lhs =
              throwError$ TCExtError hst $
              InvalidStackReference ref $ StackSize lhs
            | otherwise = pure ()
      traverse_ checkStRef pc

    safeTail :: [a] -> [a]
    safeTail (_:as) = as
    safeTail [] = []

    fitError = liftEither . first (TCExtError hst)

interpretHandler :: (ExtInstr, SomeItStack) -> EvalOp MorleyLogs ()
interpretHandler (PRINT (PrintComment pc), SomeItStack st) = do
  let getEl (Left l) = l
      getEl (Right (StackRef i)) =
        fromMaybe (error "StackRef " <> show i <> " has to exist in the stack after typechecking, but it doesn't") $
        rat st (fromIntegral i)
  modify (\s -> s {isExtState = MorleyLogs $ mconcat (map getEl pc) : unMorleyLogs (isExtState s)})
interpretHandler (TEST_ASSERT (TestAssert nm pc (instr :: T.Instr inp1 ('T.Tc 'T.CBool ': out1) )),
            SomeItStack (st :: Rec T.Value inp2)) = do
  Refl <- liftEither $ first (error "TEST_ASSERT input stack doesn't match") $ eqType @inp1 @inp2
  runInstrNoGas instr st >>= \case
    (T.VC (T.CvBool False) :& RNil) -> do
      interpretHandler (PRINT pc, SomeItStack st)
      throwError $ MichelsonFailedOther $ "TEST_ASSERT " <> nm <> " failed"
    _  -> pass

-- | Check that the optional "forall" variables are consistent if present
checkVars :: Text -> StackFn -> Either ExtError ()
checkVars t sf = case quantifiedVars sf of
  Just qs
    | varSet (inPattern sf) /= qs -> Left $ VarError t sf
  _ -> pure ()

-- | Checks the pattern in @FN@ and pushes a @ExtFrame@ onto the state
checkFn :: Text -> StackFn -> SomeHST -> TcExtFrames -> Either ExtError TcExtFrames
checkFn t sf si@(SomeHST it) nfs = do
  checkVars t sf
  second (const $ (FN t sf, si) : nfs) (checkStackType noBoundVars (inPattern sf) it)

-- |  Pops a @ExtFrame@ off the state and checks an @FN_END@ based on it
checkFnEnd :: SomeHST -> TcExtFrames -> Either ExtError BoundVars
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

lengthHST :: Num x => HST xs -> x
lengthHST (_ ::& xs) = 1 + lengthHST xs
lengthHST SNil = 0

rat :: Rec T.Value xs -> Int -> Maybe Text
rat (x :& _) 0 = Just $ show x
rat (_ :& xs) i = rat xs (i - 1)
rat RNil _ = Nothing
