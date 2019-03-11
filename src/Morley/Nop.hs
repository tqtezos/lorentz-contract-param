module Morley.Nop
  ( interpretMorleyUntyped
  , typeCheckMorleyContract
  , nopHandler
  ) where

import Data.Map.Lazy (Map, insert, lookup)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Data.Typeable ((:~:)(..))

import Michelson.Interpret
  (ContractEnv, InterpretUntypedError, InterpretUntypedResult, interpretUntyped)
import Michelson.TypeCheck
import Michelson.TypeCheck.Helpers (convergeHST, eqT')
import Michelson.Typed (converge, extractNotes, mkUType)
import Michelson.Untyped (InstrAbstract(..))
import Morley.Types

interpretMorleyUntyped
  :: Contract (Op NopInstr)
  -> Value (Op NopInstr)
  -> Value (Op NopInstr)
  -> ContractEnv NopInstr
  -> Either (InterpretUntypedError NopInstr) InterpretUntypedResult
interpretMorleyUntyped = interpretUntyped nopHandler

typeCheckMorleyContract :: Contract (Instr NopInstr) -> Either (TCError NopInstr) SomeContract
typeCheckMorleyContract = typeCheckContract nopHandler

type NopFrames = TcNopFrames NopInstr

nopHandler :: NopInstr -> NopFrames -> SomeHST -> Either (TCError NopInstr) NopFrames
nopHandler nop nfs si@(SomeHST it) =
  first (\e -> TCFailedOnInstr (NOP nop) si (nopErrorText e it)) $
    case nop of
      STACKTYPE s -> second (const nfs) (checkStackType noBoundVars s it)
      FN t sf -> checkFn t sf si nfs
      FN_END  -> second (const $ safeTail nfs) (checkFnEnd si nfs)
      _ -> Right nfs
    where
     safeTail :: [a] -> [a]
     safeTail (_:as) = as
     safeTail [] = []

-- | Various type errors possible when checking a @NopInstr@ with the
-- @nopHandler@
data NopError =
    LengthMismatch StackTypePattern Int
  | VarError Text StackFn
  | TypeMismatch StackTypePattern Int Text
  | TyVarMismatch Var Type StackTypePattern Int Text
  | FnEndMismatch (Maybe (NopInstr, SomeHST))
  | StkRestMismatch StackTypePattern SomeHST SomeHST Text
  | UnexpectedNOP NopInstr

-- | Print error messages
nopErrorText :: NopError -> HST xs -> Text
nopErrorText (LengthMismatch stk n) it = T.concat
  ["Unexpected length of stack: pattern ", show stk, " has length ", show n
  , ", but actual stack is", show it
  ]
nopErrorText (VarError t sf) _ = "In definition of " <> show t <> ": VarError " <> show sf
nopErrorText (TypeMismatch s n e) it = T.concat
  [ "TypeMismatch: Pattern ", show s, " failed on stack ", show it
  , "at index ", show n, " with \"", e, "\""
  ]
nopErrorText (TyVarMismatch v t s n e) it = T.concat
  [ "TyVarMismatch: Variable ", show v, " is bound to type ", show t
  , "but pattern ", show s, " failed on stack ", show it, "at index ", show n
  , " with \"", e, "\""
  ]
nopErrorText (FnEndMismatch n) it = "FnEndMismatch: " <> show n <> " on " <> show it
nopErrorText (UnexpectedNOP n) it = "UnexpectedNOP: " <> show n <> " on " <> show it
nopErrorText (StkRestMismatch s (SomeHST r) (SomeHST r') e) it = T.concat
  ["StkRestMismatch on stack ", show it
  , " in pattern " , show s
  , " against stacks ", show r, " and ", show r'
  , " with error: ", e
  ]

-- | Check that the optional "forall" variables are consistent if present
checkVars :: Text -> StackFn -> Either NopError ()
checkVars t sf = case quantifiedVars sf of
  Just qs
    | varSet (inPattern sf) /= qs -> Left $ VarError t sf
    | otherwise -> pure ()
  Nothing -> pure ()

-- | Checks the pattern in @FN@ and pushes a @NopFrame@ onto the state
checkFn :: Text -> StackFn -> SomeHST -> NopFrames -> Either NopError NopFrames
checkFn t sf si@(SomeHST it) nfs = do
  checkVars t sf
  second (const $ (FN t sf,si):nfs) (checkStackType noBoundVars (inPattern sf) it)

-- |  Pops a @NopFrame@ off the state and checks an @FN_END@ based on it
checkFnEnd :: SomeHST -> NopFrames -> Either NopError BoundVars
checkFnEnd (SomeHST it') (nf@(nop, (SomeHST it)):_) = case nop of
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
               -> Either NopError BoundVars
checkStackType (BoundVars vars boundStkRest) s it = go vars 0 s it
  where
    go :: Typeable xs => Map Var Type -> Int -> StackTypePattern -> HST xs
       -> Either NopError BoundVars
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
