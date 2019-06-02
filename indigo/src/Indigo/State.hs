{-
This module contains core of Indigo language:
IndigoM monad, datatype to represent its state and variables.
It also includes some convenient functions to work with state in IndigoM,
to provide rebindable syntax, and to compile Indigo to Lorentz.

IndigoM monad implements functionality of a symbolic interpreter.
During its execution Lorentz code is being generated.
-}

module Indigo.State
  ( IndigoM (..)
  , (>>=)
  , (>>)
  , return
  , iget
  , iput

  , Var
  , GenCode (..)
  , MetaData (..)
  , VarActions (..)

  , makeTopVar
  , lookupVar
  , pushNoRefMd
  , popNoRefMd

  , compileIndigo
  , compileIndigoContract
  , DestroyPrefix (..)
  ) where

import Lorentz hiding (get, return, (>>))
import Prelude hiding (return, (>>), (>>=))

import qualified Data.Kind as Kind
import Data.Typeable ((:~:)(..), eqT)
import Data.Vinyl (Rec(..))
import qualified GHC.TypeLits as Lit (ErrorMessage(..), TypeError)

import qualified Lorentz.Instr as L

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | IndigoM monad. It's basically
-- https://hackage.haskell.org/package/category-extras-0.53.5/docs/Control-Monad-Indexed-State.html
-- however, this package is not in the used lts and it doesn't compile.

-- IndigoM is basically indexed monad which
-- takes as initial an input @MetaData@
-- (which includes state of stack, with attached variables and number of allocated variables)
-- and returns new @MetaData@ and generated Lorentz code.
newtype IndigoM inp out a =
  IndigoM {runIndigoM :: MetaData inp -> (a, GenCode inp out)}

-- | Return for rebindable syntax.
return :: a -> IndigoM inp inp a
return a = IndigoM $ \md -> (a, GenCode md nop)

-- | Bind for rebindable syntax.
-- It's basically like bind for State monad but also compose
-- generated code from _m a_ and _a -> m b_.
(>>=) :: IndigoM inp out a -> (a -> IndigoM out out1 b) -> IndigoM inp out1 b
(>>=) m f = IndigoM $ \md ->
  let (a, GenCode md1 cd1) = runIndigoM m md in
  let (b, GenCode md2 cd2) = runIndigoM (f a) md1 in
  (b, GenCode md2 (cd1 # cd2))

-- | Then for rebindable syntax.
(>>) :: IndigoM inp out a -> IndigoM out out1 b -> IndigoM inp out1 b
(>>) a b = a >>= const b

-- | Get current MetaData.
iget :: IndigoM inp inp (MetaData inp)
iget = IndigoM $ \md -> (md, GenCode md nop)

-- | Put new GenCode.
iput :: GenCode inp out -> IndigoM inp out ()
iput gc = IndigoM $ \_ -> ((), gc)

----------------------------------------------------------------------------
-- Indigo state and variables
----------------------------------------------------------------------------

-- | Variable whcih is exposed to a user.
-- It stores number of variable, its phantom type
-- represent type of a variable.
newtype Var a = Var Word

-- | Stack element of symbolic interpreter.
-- It holds either a variable which refers to this element
-- or just NoRef which indicates that there are no references
-- to this element.
data StkEl a
  = NoRef
  | Typeable a => Ref (Var a)

-- | Stack of symbolic interpreter.
type StackVars (stk :: [Kind.Type]) = Rec StkEl stk

-- | Initial state of IndigoM.
data MetaData stk = MetaData
  { mdStack :: StackVars stk
  -- ^ Stack of symbolic interpreter.
  , mdRefCount :: Word
  -- ^ Number of allocated variables.
  }

-- | Resulting state of IndigoM.
data GenCode inp out = GenCode
  { gcMeta :: MetaData out
  -- ^ Interpreter meta data.
  , gcCode  :: inp :-> out
  -- ^ Generated Lorentz code.
  }

-- | Create a variable which references to the top of a stack.
makeTopVar :: Typeable x => IndigoM (x & inp) (x & inp) (Var x)
makeTopVar = do
  MetaData s ref <- iget
  case s of
    (Ref v@(Var _) :& _) -> return v
    NoRef :& xs          ->
      let var = Var ref in
      iput (GenCode (MetaData (Ref var :& xs) (ref + 1)) nop) >>
      return var

-- | Lorentz code to get access to a variable
-- and to set to it new value.
data VarActions a s = VarActions
  { vaGet :: s :-> a & s
  -- ^ Get a variable on the top of a stack.
  , vaSet :: a & s :-> s
  -- ^ Set a new value to variable.
  }

-- | Look up variable on the stack and provide actions
-- to get and set it.
lookupVar :: forall a inp . Typeable a => Var a -> StackVars inp -> VarActions a inp
lookupVar (Var varNum) RNil =
  error $ "Either leaked variable or manually created one. Ref #" <> show varNum
lookupVar v vars@(_ :& _) = lookupVarNonEmpty v vars

-- Specification of lookupVar for a non empty stack.
lookupVarNonEmpty :: forall x s s' a .
  ( Typeable x
  , s ~ (a & s')
  )
  => Var x -> StackVars s -> VarActions x s
lookupVarNonEmpty x@(Var varNum) (cv :& s') = case cv of
  Ref (Var varNum1)
    | varNum == varNum1 ->
        case eqT @x @a of
          Just Refl -> VarActions L.dup (L.swap # L.drop)
          Nothing   -> error $ "Invalid type of a varible with ref #" <> show varNum
  _ -> buildVA $ lookupVar x s'
  where
    buildVA :: VarActions x s' -> VarActions x s
    buildVA (VarActions g s) = VarActions (L.dip g # L.swap) (L.swap # dip s)

-- | Push a new stack element without reference to it.
pushNoRefMd :: MetaData inp -> MetaData (a & inp)
pushNoRefMd (MetaData xs ref) = MetaData (NoRef :& xs) ref

-- | Remove top element of the stack.
-- It's supposed that no variables refer to the top of the stack.
popNoRefMd :: MetaData (a & inp) -> MetaData inp
popNoRefMd (MetaData (NoRef :& xs) ref) = MetaData xs ref
popNoRefMd (MetaData (Ref (Var num) :& _) _) =
  error $ "You try to pop stack element, which is referenced by some variable #" <> show num

----------------------------------------------------------------------------
-- Compilation
----------------------------------------------------------------------------

-- | Compile Indigo code to Lorentz.
compileIndigo
  :: (Typeable param, Typeable st)
  => (Var param -> Var st -> IndigoM '[param, st] out ())
  -> ('[param, st] :-> out)
compileIndigo code = do
  let varSt = Var 0
  let varParam = Var 1
  let vars = Ref varParam :& Ref varSt :& RNil
  let md = MetaData vars 2
  gcCode $ snd $ runIndigoM (code varParam varSt) md

-- | Compile Indigo code to Lorentz contract.
-- Drop elements from the stack to return only [Operation] and Storage.
compileIndigoContract
  :: forall param st out .
  ( Typeable param, Typeable st
  , DestroyPrefix out '[st]
  )
  => (Var param -> Var st -> IndigoM '[param, st] out ())
  -> Contract param st
compileIndigoContract code = do
  let varSt = Var 0
  let varParam = Var 1
  let vars = Ref varParam :& Ref varSt :& RNil
  let md = MetaData vars 2
  let loretzCode =
        gcCode $ destroyPrefix @out @'[st] $
          snd $ runIndigoM (code varParam varSt) md
  unpair # loretzCode # nil # pair

-- This typeclass is needed to destroy variables when we
-- leave a scope where they were created.
class DestroyPrefix from to where
  destroyPrefix :: GenCode inp from -> GenCode inp to

-- DestroyPrefix' and Decide machinery is neede to prevent overlapping instances.
instance DestroyPrefix' (Decide from to) from to => DestroyPrefix from to where
  destroyPrefix = destroyPrefix' @(Decide from to)

destroyTop :: GenCode inp (x & xs) -> GenCode inp xs
destroyTop (GenCode (MetaData (_ :& vars) ref) cd) = GenCode (MetaData vars ref) (cd # L.drop)

class DestroyPrefix' decision from to where
  destroyPrefix' :: GenCode inp from -> GenCode inp to

instance DestroyPrefix' 'Done from from where
  destroyPrefix' = id

instance DestroyPrefix' d xs to => DestroyPrefix' ('Skip d) (x ': xs) to where
  destroyPrefix' = destroyPrefix' @d @xs @to . destroyTop

data Decision = Done | Skip Decision

type family Decide from to :: Decision where
  Decide from from = 'Done
  Decide (_ ': rs) to = 'Skip (Decide rs to)
  Decide '[] _ = Lit.TypeError ('Lit.Text "Prefix can't be destroyed. Not matched stacks")
