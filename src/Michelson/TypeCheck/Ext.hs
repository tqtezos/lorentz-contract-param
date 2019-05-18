-- | Type-checking of Morley extension.
module Michelson.TypeCheck.Ext
  ( typeCheckExt
  ) where

import Control.Lens ((%=))
import Control.Monad.Except (MonadError, liftEither, throwError)
import Data.Map.Lazy (Map, insert, lookup)
import Data.Singletons (Sing)
import Data.Typeable ((:~:)(..))

import Michelson.ErrorPos
import Michelson.TypeCheck.Error
import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.TypeCheck
import Michelson.TypeCheck.Types
import Michelson.Typed (converge, extractNotes, mkUType)
import qualified Michelson.Typed as T
import Michelson.Untyped
  (CT(..), ExpandedOp, StackFn, TyVar(..), Type, Var, VarAnn, inPattern, outPattern,
  quantifiedVars, varSet)
import qualified Michelson.Untyped as U
import Util.Peano (Sing(SS, SZ))

type TypeCheckListHandler inp =
     [U.ExpandedOp]
  -> HST inp
  -> TypeCheck (SomeInstr inp)

typeCheckExt
  :: forall s.
     (Typeable s)
  => TypeCheckListHandler s
  -> U.ExpandedExtInstr
  -> HST s
  -> TypeCheckInstr (SomeInstr s)
typeCheckExt typeCheckListH ext hst = do
  instrPos <- ask
  case ext of
    U.STACKTYPE s -> liftExtError hst $ nopSomeInstr <$ checkStackType noBoundVars s hst
    U.FN t sf op  -> checkFn typeCheckListH t sf op hst
    U.UPRINT pc   -> verifyPrint pc <&> \tpc -> toSomeInstr (T.PRINT tpc)
    U.UTEST_ASSERT U.TestAssert{..} -> do
      verifyPrint tassComment
      _ :/ si <- lift $ typeCheckListH tassInstrs hst
      case si of
        AnyOutInstr _ -> throwError $ TCExtError (SomeHST hst) instrPos $ TestAssertError
                         "TEST_ASSERT has to return Bool, but it always fails"
        instr ::: (((_ :: (Sing b, T.Notes b, VarAnn)) ::& (_ :: HST out1))) -> do
          Refl <- liftEither $
                    first (const $ TCExtError (SomeHST hst) instrPos $
                           TestAssertError "TEST_ASSERT has to return Bool, but returned something else") $
                      eqType @b @('T.Tc 'CBool)
          tcom <- verifyPrint tassComment
          pure . toSomeInstr $ T.TEST_ASSERT $ T.TestAssert tassName tcom instr
        _ ->
          throwError $ TCExtError (SomeHST hst) instrPos $
            TestAssertError "TEST_ASSERT has to return Bool, but the stack is empty"
  where
    verifyPrint :: U.PrintComment -> TypeCheckInstr (T.PrintComment s)
    verifyPrint (U.PrintComment pc) = do
      let checkStRef (Left txt)             = pure $ Left txt
          checkStRef (Right (U.StackRef i)) = Right <$> createStackRef i hst
      T.PrintComment <$> traverse checkStRef pc

    toSomeInstr ext' = hst :/ T.Ext ext' ::: hst
    nopSomeInstr = hst :/ T.Nop ::: hst

liftExtError :: Typeable s => HST s -> Either ExtError a -> TypeCheckInstr a
liftExtError hst ei = do
  instrPos <- ask
  liftEither $ first (TCExtError (SomeHST hst) instrPos) ei

-- | Check that the optional "forall" variables are consistent if present
checkVars :: Text -> StackFn -> Either ExtError ()
checkVars t sf = case quantifiedVars sf of
  Just qs
    | varSet (inPattern sf) /= qs -> Left $ VarError t sf
  _ -> pass

-- | Executes function body, pushing @ExtFrame@ onto the state and checks
-- the pattern in @FN@.
checkFn
  :: Typeable inp
  => TypeCheckListHandler inp
  -> Text
  -> StackFn
  -> [ExpandedOp]
  -> HST inp
  -> TypeCheckInstr (SomeInstr inp)
checkFn typeCheckListH t sf body inp = do
  vars <- checkStart inp
  res@(_ :/ instr) <- lift $ typeCheckListH body inp
  case instr of
    _ ::: out -> checkEnd vars out
    AnyOutInstr _ -> pass
  return res
  where
    checkStart hst = do
      liftExtError hst $ checkVars t sf
      vars <- liftExtError hst $ checkStackType noBoundVars (inPattern sf) hst
      tcExtFramesL %= (vars :)
      return vars

    checkEnd :: Typeable out => BoundVars -> HST out -> TypeCheckInstr ()
    checkEnd vars out = liftExtError out $
      void $ checkStackType vars (outPattern sf) out

-- | Check that a @StackTypePattern@ matches the type of the current stack
checkStackType
  :: Typeable xs
  => BoundVars
  -> U.StackTypePattern
  -> HST xs
  -> Either ExtError BoundVars
checkStackType (BoundVars vars boundStkRest) s hst = go vars 0 s hst
  where
    go :: Typeable xs => Map Var Type -> Int -> U.StackTypePattern -> HST xs
       -> Either ExtError BoundVars
    go m _ U.StkRest sr = case boundStkRest of
      Nothing -> pure $ BoundVars m (Just $ SomeHST sr)
      Just si@(SomeHST sr') ->
        bimap (StkRestMismatch s (SomeHST sr) si)
              (const $ BoundVars m (Just si))
              (eqHST sr sr')
    go m _ U.StkEmpty SNil = pure $ BoundVars m Nothing
    go _ _ U.StkEmpty _    = Left $ LengthMismatch s
    go _ _ _ SNil        = Left $ LengthMismatch s
    go m n (U.StkCons (TyCon t) ts) ((xt, xann, _) ::& xs) = do
      tann <- first (TypeMismatch s n . ExtractionTypeMismatch) (extractNotes t xt)
      void $ first (TypeMismatch s n . AnnError) (converge tann xann)
      go m (n + 1) ts xs
    go m n (U.StkCons (VarID v) ts) ((xt, xann, _) ::& xs) =
      case lookup v m of
        Nothing -> let t = mkUType xt xann in go (insert v t m) (n + 1) ts xs
        Just t -> do
          tann <- first (TyVarMismatch v t s n . ExtractionTypeMismatch) (extractNotes t xt)
          void $ first (TyVarMismatch v t s n . AnnError) (converge tann xann)
          go m (n + 1) ts xs

-- | Create stack reference accessing element with a given index.
--
-- Fails when index is too large for the given stack.
createStackRef
  :: (MonadError TCError m, MonadReader InstrCallStack m, Typeable s)
  => Natural -> HST s -> m (T.StackRef s)
createStackRef idx hst =
  case doCreate (hst, idx) of
    Just sr -> pure sr
    Nothing -> do
      instrPos <- ask
      throwError $
        TCExtError (SomeHST hst) instrPos $
          InvalidStackReference (U.StackRef idx) (StackSize $ lengthHST hst)
  where
    doCreate :: forall s. (HST s, Natural) -> Maybe (T.StackRef s)
    doCreate = \case
      (SNil, _) -> Nothing
      ((_ ::& _), 0) -> Just (T.StackRef SZ)
      ((_ ::& st), i) -> do
        T.StackRef ns <- doCreate (st, i - 1)
        return $ T.StackRef (SS ns)
