{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Module, containing restrictions imposed by instruction or value scope.

module Michelson.Typed.Scope
  ( AllowBigMap
  , HasNoBigMap
  , HasNoOp
  , ForbidBigMap
  , ForbidOp
  , BigMapPresence (..)
  , OpPresence (..)
  , BigMapConstraint
  , checkOpPresence
  , checkBigMapPresence
  , checkBigMapConstraint
  , opAbsense
  , bigMapAbsense
  , forbiddenOp
  , forbiddenBigMap
  , bigMapConstrained
  ) where

import Data.Constraint (Dict(..))
import Data.Singletons (SingI(..))
import Data.Type.Bool  (type (||))
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

-- | Whether this type contains 'TBigMap' type.
--
-- In some scopes (constants, parameters) appearing for big_map type
-- is prohibited. It is permitted in toplevel left element of storage pair.
-- Big_maps in input/output of lambdas are allowed without limits though.
type family ContainsBigMap (t :: T) :: Bool where
  ContainsBigMap ('Tc _) = 'False
  ContainsBigMap 'TKey = 'False
  ContainsBigMap 'TUnit = 'False
  ContainsBigMap 'TSignature = 'False
  ContainsBigMap ('TOption t) = ContainsBigMap t
  ContainsBigMap ('TList t) = ContainsBigMap t
  ContainsBigMap ('TSet _) = 'False
  ContainsBigMap 'TOperation = 'False
  ContainsBigMap ('TContract t) = ContainsBigMap t
  ContainsBigMap ('TPair a b) = ContainsBigMap a || ContainsBigMap b
  ContainsBigMap ('TOr a b) = ContainsBigMap a || ContainsBigMap b
  ContainsBigMap ('TLambda _ _) = 'False
  ContainsBigMap ('TMap _ v) = ContainsBigMap v
  ContainsBigMap ('TBigMap _ _) = 'True

-- | Constraint which ensures that operation type does not appear in a given type.
--
-- Not just a type alias in order to be able to partially apply it
-- (e.g. in 'Each').
class (ContainsOp t ~ 'False) => HasNoOp t
instance (ContainsOp t ~ 'False) => HasNoOp t

-- | Constraint which ensures that bigmap does not appear in a given type.
class (ContainsBigMap t ~ 'False) => HasNoBigMap t
instance (ContainsBigMap t ~ 'False) => HasNoBigMap t

-- | Type family to check if t is ill-typed contract storage
--   (it contains bigmap not on the left of its toplevel pair)
--
-- Used in @BigMapConstraint@
type family BadBigMapPair t :: Bool where
  BadBigMapPair ('TPair ('TBigMap _ v) b) =
    ContainsBigMap v || ContainsBigMap b
  BadBigMapPair t = ContainsBigMap t

-- | Constraint which ensures, that @t@ can be used as type of contract storage
--   so it optionally has bigmap only on the left of its toplevel pair
type BigMapConstraint t = BadBigMapPair t ~ 'False

-- | Report a human-readable error about 'TOperation' at a wrong place.
type family FailOnOperationFound (enabled :: Bool) :: Constraint where
  FailOnOperationFound 'True =
    TypeError ('Text "Operations are not allowed in this scope")
  FailOnOperationFound 'False = ()

-- | Report a human-readable error about 'TBigMap' at a wrong place.
type family FailOnBigMapFound (enabled :: Bool) :: Constraint where
  FailOnBigMapFound 'True =
    TypeError ('Text "BigMaps are not allowed in this scope")
  FailOnBigMapFound 'False = ()

-- | This is like 'HasNoOp', it raises a more human-readable error
-- when @t@ type is concrete, but you cannot easily extract a proof
-- of no-operation-presence from it.
--
-- Use it in our eDSL.
type ForbidOp t = FailOnOperationFound (ContainsOp t)

type ForbidBigMap t = FailOnBigMapFound (ContainsBigMap t)

type AllowBigMap t = FailOnBigMapFound (BadBigMapPair t)

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

forbiddenBigMap
  :: forall t a.
     (SingI t, ForbidBigMap t)
  => (HasNoBigMap t => a)
  -> a
forbiddenBigMap a =
  case checkBigMapPresence (sing @t) of
    BigMapAbsent -> a
    BigMapPresent -> error "impossible"

checkBigMapConstraint
  :: forall t a.
     (SingI t, AllowBigMap t)
  => (BigMapConstraint t => a)
  -> a
checkBigMapConstraint a =
  case bigMapConstrained (sing @t) of
    Just Dict -> a
    Nothing -> error "impossible"

-- | Whether the type contains 'TOperation', with proof.
data OpPresence (t :: T)
  = ContainsOp t ~ 'True => OpPresent
  | ContainsOp t ~ 'False => OpAbsent

data BigMapPresence (t :: T)
  = ContainsBigMap t ~ 'True => BigMapPresent
  | ContainsBigMap t ~ 'False => BigMapAbsent

-- @rvem: IMO, generalization of OpPresence and BigMapPresence to
-- TPresence is not worth it, due to the fact that
-- it will require more boilerplate in checkTPresence implementation
-- than it is already done in checkOpPresence and checkBigMapPresence

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
  STLambda _ _ -> OpAbsent
  STMap _ v -> case checkOpPresence v of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent
  STBigMap _ v -> case checkOpPresence v of
    OpPresent -> OpPresent
    OpAbsent -> OpAbsent

-- | Check at runtime whether the given type contains 'TBigMap'.
checkBigMapPresence :: Sing (ty :: T) -> BigMapPresence ty
checkBigMapPresence = \case
  -- More boilerplate to boilerplate god.
  STc _ -> BigMapAbsent
  STKey -> BigMapAbsent
  STSignature -> BigMapAbsent
  STUnit -> BigMapAbsent
  STOption t -> case checkBigMapPresence t of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STList t -> case checkBigMapPresence t of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STSet _ -> BigMapAbsent
  STOperation -> BigMapAbsent
  STContract t -> case checkBigMapPresence t of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STPair a b -> case (checkBigMapPresence a, checkBigMapPresence b) of
    (BigMapPresent, _) -> BigMapPresent
    (_, BigMapPresent) -> BigMapPresent
    (BigMapAbsent, BigMapAbsent) -> BigMapAbsent
  STOr a b -> case (checkBigMapPresence a, checkBigMapPresence b) of
    (BigMapPresent, _) -> BigMapPresent
    (_, BigMapPresent) -> BigMapPresent
    (BigMapAbsent, BigMapAbsent) -> BigMapAbsent
  STLambda _ _ -> BigMapAbsent
  STMap _ v -> case checkBigMapPresence v of
    BigMapPresent -> BigMapPresent
    BigMapAbsent -> BigMapAbsent
  STBigMap _ _ ->
    BigMapPresent

-- | Check at runtime that the given type does not contain 'TOperation'.
opAbsense :: Sing (t :: T) -> Maybe (Dict $ HasNoOp t)
opAbsense s = case checkOpPresence s of
  OpPresent -> Nothing
  OpAbsent -> Just Dict

-- | Check at runtime that the given type does not containt 'TBigMap'
bigMapAbsense :: Sing (t :: T) -> Maybe (Dict $ HasNoBigMap t)
bigMapAbsense s = case checkBigMapPresence s of
  BigMapPresent -> Nothing
  BigMapAbsent -> Just Dict

-- | Check at runtime that the given type optionally has bigmap
--   only on the left of its toplevel pair, which is actuall constraint for
--   bigmap appearance in the storage
bigMapConstrained :: Sing (t :: T) -> Maybe (Dict $ BigMapConstraint t)
bigMapConstrained = \case
  -- Yet another bunch of boilerplate
  -- We have to make pattern matching on first argument of STPair
  -- to prove, that BigMap appears only on its leftmost position,
  -- and also make pattern matching on all Sing constructors
  -- to prove that TPair appears only in STPair
  STPair (STBigMap _ v) b ->
    case (bigMapAbsense v, bigMapAbsense b) of
      (Just Dict, Just Dict) -> Just Dict
      _ -> Nothing
  STPair (STc _) b -> case bigMapAbsense b of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STPair STKey b -> case bigMapAbsense b of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STPair STUnit b -> case bigMapAbsense b of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STPair STSignature b -> case bigMapAbsense b of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STPair (STOption a) b -> case (bigMapAbsense a, bigMapAbsense b) of
    (Just Dict, Just Dict) -> Just Dict
    _ -> Nothing
  STPair (STList a) b -> case (bigMapAbsense a, bigMapAbsense b) of
    (Just Dict, Just Dict) -> Just Dict
    _ -> Nothing
  STPair (STSet _) b -> case bigMapAbsense b of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STPair STOperation b ->  case bigMapAbsense b of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STPair (STContract a) b -> case (bigMapAbsense a, bigMapAbsense b) of
    (Just Dict, Just Dict) -> Just Dict
    _ -> Nothing
  STPair (STPair a b) c ->
    case (bigMapAbsense a, bigMapAbsense b, bigMapAbsense c) of
      (Just Dict, Just Dict, Just Dict) -> Just Dict
      _ -> Nothing
  STPair (STOr a b) c ->
    case (bigMapAbsense a, bigMapAbsense b, bigMapAbsense c) of
      (Just Dict, Just Dict, Just Dict) -> Just Dict
      _ -> Nothing
  STPair (STLambda _ _) c ->
    case bigMapAbsense c of
      Just Dict -> Just Dict
      Nothing -> Nothing
  STPair (STMap _ v) b -> case (bigMapAbsense v, bigMapAbsense b) of
    (Just Dict, Just Dict) -> Just Dict
    _ -> Nothing
  STc _ -> Just Dict
  STKey -> Just Dict
  STSignature -> Just Dict
  STUnit -> Just Dict
  STOption t -> case bigMapAbsense t of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STList t -> case bigMapAbsense t of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STSet _ -> Just Dict
  STOperation -> Just Dict
  STContract t -> case bigMapAbsense t of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STOr a b -> case (bigMapAbsense a, bigMapAbsense b) of
    (Just Dict, Just Dict) -> Just Dict
    _ -> Nothing
  STLambda _ _ -> Just Dict
  STMap _ v -> case bigMapAbsense v of
    Just Dict -> Just Dict
    Nothing -> Nothing
  STBigMap _ _ -> Nothing
