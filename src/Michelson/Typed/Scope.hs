{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Module, containing restrictions imposed by instruction or value scope.

module Michelson.Typed.Scope
  ( HasNoOp
  , ForbidOp
  , OpPresence (..)
  , checkOpPresence
  , opAbsense
  , forbiddenOp
  ) where

import Data.Constraint (Dict(..))
import Data.Singletons (SingI(..))
import Data.Type.Bool (type (||))
import GHC.TypeLits (ErrorMessage(..), TypeError)

import Michelson.Typed.Sing (Sing(..))
import Michelson.Typed.T (T(..))

-- | Whether this type contains 'TOperation' type.
--
-- In some scopes (constants, parameters, storage) appearing for operation type
-- is prohibited.
-- Operations in input/output of lambdas are allowed without limits though.
type family ContainsOp (t :: T) :: Bool where
  ContainsOp ('Tc _) = 'False
  ContainsOp 'TKey = 'False
  ContainsOp 'TUnit = 'False
  ContainsOp 'TSignature = 'False
  ContainsOp ('TOption t) = ContainsOp t
  ContainsOp ('TList t) = ContainsOp t
  ContainsOp ('TSet _) = 'False
  ContainsOp 'TOperation = 'True
  ContainsOp ('TContract t) = ContainsOp t
  ContainsOp ('TPair a b) = ContainsOp a || ContainsOp b
  ContainsOp ('TOr a b) = ContainsOp a || ContainsOp b
  ContainsOp ('TLambda _ _) = 'False
  ContainsOp ('TMap _ v) = ContainsOp v
  ContainsOp ('TBigMap _ v) = ContainsOp v

-- | Contraint which ensures that operation type does not appear in a given type.
--
-- Not just a type alias in order to be able to partially apply it
-- (e.g. in 'Each').
class (ContainsOp t ~ 'False) => HasNoOp t
instance (ContainsOp t ~ 'False) => HasNoOp t

-- | Report a human-readable error about 'TOperation' at a wrong place.
type family FailOnOperationFound (enabled :: Bool) :: Constraint where
  FailOnOperationFound 'True =
    TypeError ('Text "Operations are not allowed in this scope")
  FailOnOperationFound 'False = ()

-- | This is like 'HasNoOp', it raises a more human-readable error
-- when @t@ type is concrete, but you cannot easily extract a proof
-- of no-operation-presence from it.
--
-- Use it in our eDSL.
type ForbidOp t = FailOnOperationFound (ContainsOp t)

-- | Reify 'HasNoOp' contraint from 'ForbidOp'.
forbiddenOp
  :: forall t a.
     (SingI t, ForbidOp t)
  => (HasNoOp t => a)
  -> a
forbiddenOp a =
  -- It's not clear now to proof GHC that @HasNoOp t@ is implication of
  -- @ForbidOp t@, so we use @error@ below and also disable
  -- "-Wredundant-constraints" extension.
  case checkOpPresence (sing @t) of
    OpAbsent -> a
    OpPresent -> error "impossible"

-- | Whether the type contains 'TOperation', with proof.
data OpPresence (t :: T)
  = ContainsOp t ~ 'True => OpPresent
  | ContainsOp t ~ 'False => OpAbsent

-- | Check at runtime whether the given type contains 'TOperation'.
checkOpPresence :: Sing (ty :: T) -> OpPresence ty
checkOpPresence = \case
  -- This is a sad amount of boilerplate, but at least
  -- there is no chance to make a mistake in it.
  -- We can't do in a simpler way while requiring only @Sing ty@ / @SingI ty@,
  -- and a more complex constraint would be too unpleasant and confusing to
  -- propagate everywhere.
  STc _ -> OpAbsent
  STKey -> OpAbsent
  STSignature -> OpAbsent
  STUnit -> OpAbsent
  STOption t -> case checkOpPresence t of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STList t -> case checkOpPresence t of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STSet _ -> OpAbsent
  STOperation -> OpPresent
  STContract t -> case checkOpPresence t of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STPair a b -> case (checkOpPresence a, checkOpPresence b) of
    (OpPresent, _) -> OpPresent
    (_, OpPresent) -> OpPresent
    (OpAbsent, OpAbsent) -> OpAbsent
  STOr a b -> case (checkOpPresence a, checkOpPresence b) of
    (OpPresent, _) -> OpPresent
    (_, OpPresent) -> OpPresent
    (OpAbsent, OpAbsent) -> OpAbsent
  STLambda _ _ ->
    OpAbsent
  STMap _ v -> case checkOpPresence v of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STBigMap _ v -> case checkOpPresence v of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent

-- | Check at runtime that the given type does not contain 'TOperation'.
opAbsense :: Sing (t :: T) -> Maybe (Dict $ HasNoOp t)
opAbsense s = case checkOpPresence s of
  OpPresent -> Nothing
  OpAbsent -> Just Dict
