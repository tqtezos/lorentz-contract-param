{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Common Michelson macros defined using Lorentz syntax.
module Lorentz.Macro
  ( -- * Compare
    IfCmpXConstraints
  , eq
  , neq
  , lt
  , gt
  , le
  , ge
  , ifEq0
  , ifGe0
  , ifGt0
  , ifLe0
  , ifLt0
  , ifNeq0
  , ifEq
  , ifGe
  , ifGt
  , ifLe
  , ifLt
  , ifNeq

  -- * Fail
  , fail_

  -- * Assertion macros
  , assert
  , assertEq0
  , assertNeq0
  , assertLt0
  , assertGt0
  , assertLe0
  , assertGe0
  , assertEq
  , assertNeq
  , assertLt
  , assertGt
  , assertLe
  , assertGe
  , assertNone
  , assertSome
  , assertLeft
  , assertRight

  -- * Syntactic Conveniences
  , dipX
  , cloneX
  , duupX
  , caar
  , cadr
  , cdar
  , cddr
  , ifRight
  , ifSome
  , mapCar
  , mapCdr
  , papair
  , ppaiir
  , unpair
  , setCar
  , setCdr

  -- * Additional Morley macros
  , View (..)
  , Void_ (..)
  , view_
  , void_
  , mkVoid
  ) where

import Prelude hiding (compare, some, swap)

import Data.Vinyl.TypeLevel (Nat(..))
import qualified Data.Kind as Kind
import GHC.TypeNats (type (-))
import qualified GHC.TypeNats as GHC (Nat)

import Lorentz.Instr
import Lorentz.Base
import Lorentz.Constraints
import Lorentz.Arith
import Lorentz.Value
import Michelson.Typed.Arith
import Util.Peano

----------------------------------------------------------------------------
-- Compare
----------------------------------------------------------------------------

eq :: (ArithOpHs Compare n m, UnaryArithOpHs Eq' (ArithResHs Compare n m))
   => n & m & s :-> (UnaryArithResHs Eq' (ArithResHs Compare n m)) & s
eq = compare # eq0

neq :: (ArithOpHs Compare n m, UnaryArithOpHs Neq (ArithResHs Compare n m))
   => n & m & s :-> (UnaryArithResHs Neq (ArithResHs Compare n m)) & s
neq = compare # neq0

gt :: (ArithOpHs Compare n m, UnaryArithOpHs Gt (ArithResHs Compare n m))
   => n & m & s :-> (UnaryArithResHs Gt (ArithResHs Compare n m)) & s
gt = compare # gt0

le :: (ArithOpHs Compare n m, UnaryArithOpHs Le (ArithResHs Compare n m))
   => n & m & s :-> (UnaryArithResHs Le (ArithResHs Compare n m)) & s
le = compare # le0

ge :: (ArithOpHs Compare n m, UnaryArithOpHs Ge (ArithResHs Compare n m))
   => n & m & s :-> (UnaryArithResHs Ge (ArithResHs Compare n m)) & s
ge = compare # ge0

lt :: (ArithOpHs Compare n m, UnaryArithOpHs Lt (ArithResHs Compare n m))
   => n & m & s :-> (UnaryArithResHs Lt (ArithResHs Compare n m)) & s
lt = compare # lt0

type IfCmp0Constraints a op =
  (UnaryArithOpHs op a, (UnaryArithResHs op a ~ Bool))

ifEq0
  :: (IfCmp0Constraints a Eq')
  => (s :-> s') -> (s :-> s') -> (a & s :-> s')
ifEq0 l r = eq0 # if_ l r

ifNeq0
  :: (IfCmp0Constraints a Neq)
  => (s :-> s') -> (s :-> s') -> (a & s :-> s')
ifNeq0 l r = neq0 # if_ l r

ifLt0
  :: (IfCmp0Constraints a Lt)
  => (s :-> s') -> (s :-> s') -> (a & s :-> s')
ifLt0 l r = lt0 # if_ l r

ifGt0
  :: (IfCmp0Constraints a Gt)
  => (s :-> s') -> (s :-> s') -> (a & s :-> s')
ifGt0 l r = gt0 # if_ l r

ifLe0
  :: (IfCmp0Constraints a Le)
  => (s :-> s') -> (s :-> s') -> (a & s :-> s')
ifLe0 l r = le0 # if_ l r

ifGe0
  :: (IfCmp0Constraints a Ge)
  => (s :-> s') -> (s :-> s') -> (a & s :-> s')
ifGe0 l r = ge0 # if_ l r

type IfCmpXConstraints a b op =
  (Typeable a, Typeable b, ArithOpHs Compare a b
  , UnaryArithOpHs op (ArithResHs Compare a b)
  , UnaryArithResHs op (ArithResHs Compare a b) ~ Bool)

ifEq
  :: (IfCmpXConstraints a b Eq')
  => (s :-> s') -> (s :-> s') -> (a & b & s :-> s')
ifEq l r = eq # if_ l r

ifNeq
  :: (IfCmpXConstraints a b Neq)
  => (s :-> s') -> (s :-> s') -> (a & b & s :-> s')
ifNeq l r = neq # if_ l r

ifLt
  :: (IfCmpXConstraints a b Lt)
  => (s :-> s') -> (s :-> s') -> (a & b & s :-> s')
ifLt l r = lt # if_ l r

ifGt
  :: (IfCmpXConstraints a b Gt)
  => (s :-> s') -> (s :-> s') -> (a & b & s :-> s')
ifGt l r = gt # if_ l r

ifLe
  :: (IfCmpXConstraints a b Le)
  => (s :-> s') -> (s :-> s') -> (a & b & s :-> s')
ifLe l r = le # if_ l r

ifGe
  :: (IfCmpXConstraints a b Ge)
  => (s :-> s') -> (s :-> s') -> (a & b & s :-> s')
ifGe l r = ge # if_ l r

----------------------------------------------------------------------------
-- Fail
----------------------------------------------------------------------------

fail_ :: a :-> c
fail_ = unit # failWith

----------------------------------------------------------------------------
-- Assertions
----------------------------------------------------------------------------

assert :: Bool & s :-> s
assert = if_ nop fail_

assertEq0 :: IfCmp0Constraints a Eq' => a & s :-> s
assertEq0 = ifEq0 nop fail_

assertNeq0 :: IfCmp0Constraints a Neq => a & s :-> s
assertNeq0 = ifNeq0 nop fail_

assertLt0 :: IfCmp0Constraints a Lt => a & s :-> s
assertLt0 = ifLt0 nop fail_

assertGt0 :: IfCmp0Constraints a Gt => a & s :-> s
assertGt0 = ifGt0 nop fail_

assertLe0 :: IfCmp0Constraints a Le => a & s :-> s
assertLe0 = ifLe0 nop fail_

assertGe0 :: IfCmp0Constraints a Ge => a & s :-> s
assertGe0 = ifGe0 nop fail_

assertEq :: IfCmpXConstraints a b Eq' => a & b & s :-> s
assertEq = ifEq nop fail_

assertNeq :: IfCmpXConstraints a b Neq => a & b & s :-> s
assertNeq = ifNeq nop fail_

assertLt :: IfCmpXConstraints a b Lt => a & b & s :-> s
assertLt = ifLt nop fail_

assertGt :: IfCmpXConstraints a b Gt => a & b & s :-> s
assertGt = ifGt nop fail_

assertLe :: IfCmpXConstraints a b Le => a & b & s :-> s
assertLe = ifLe nop fail_

assertGe :: IfCmpXConstraints a b Ge => a & b & s :-> s
assertGe = ifGe nop fail_

assertNone :: Maybe a & s :-> s
assertNone = ifNone nop fail_

assertSome :: Maybe a & s :-> a & s
assertSome = ifNone fail_ nop

assertLeft :: Either a b & s :-> a & s
assertLeft = ifLeft nop fail_

assertRight :: Either a b & s :-> b & s
assertRight = ifLeft fail_ nop

----------------------------------------------------------------------------
-- Syntactic Conveniences
----------------------------------------------------------------------------

type family Above (n :: Peano) s where
  Above  'Z _s = '[]
  Above ('S n) (a ': s) = (a ': Above n s)

class DipX (n :: Peano) (head :: [Kind.Type]) (s :: [Kind.Type]) (s' :: [Kind.Type]) where
  dipXImpl :: s :-> s' -> head ++ s :-> head ++ s'
instance DipX 'Z '[] s s' where
  dipXImpl op = op
instance DipX n h s s' => DipX ('S n) (a ': h) s s' where
  dipXImpl = dip . dipXImpl @n @h

-- Type family, that is necessary for DipX
type family (++) (l :: [a]) (r :: [a]) where
  '[] ++ r = r
  (l ': ls) ++ r = l ': (ls ++ r)

-- | @DII+P@ macro. For example, `dipX @3` is `DIIIP`.
dipX
  :: forall (n :: GHC.Nat) inp out s s'.
  (DipX (ToPeano n) (Above (ToPeano n) inp) s s'
  , ((Above (ToPeano n) inp) ++ s) ~ inp
  , ((Above (ToPeano n) inp) ++ s') ~ out)
  => (s :-> s') -> inp :-> out
dipX = dipXImpl @(ToPeano n) @(Above (ToPeano n) inp)

class CloneX (n :: Peano) a s where
  type CloneXT n a s :: [Kind.Type]
  cloneXImpl :: a & s :-> CloneXT n a s
instance CloneX 'Z a s where
  type CloneXT 'Z a s = a & s
  cloneXImpl = nop
instance (CloneX n a s) => CloneX ('S n) a s where
  type CloneXT ('S n) a s = a ': CloneXT n a s
  cloneXImpl = dup # dip (cloneXImpl @n)

-- | Duplicate the top of the stack @n@ times.
--
-- For example, `cloneX @3` has type `a & s :-> a & a & a & a & s`.
cloneX
  :: forall (n :: GHC.Nat) a s. CloneX (ToPeano n) a s
  => a & s :-> CloneXT (ToPeano n) a s
cloneX = cloneXImpl @(ToPeano n)

class DuupX (n :: Peano) (l :: [Kind.Type]) (r :: [Kind.Type]) (a :: Kind.Type) (s :: [Kind.Type]) where
  duupXImpl :: l ++ r ++ (a & s) :-> l ++ '[a] ++ r ++ (a & s)
instance ( DipX n l (a & s) (a & a & s)
         , (l ++ (a & s)) ~ (l ++ '[] ++ (a & s))
         , (l ++ (a & a & s)) ~ (l ++ '[a] ++ '[] ++ (a & s))
         ) => DuupX n l '[] a s where
  duupXImpl = dipXImpl @n @l @(a & s) @(a & a & s) dup
instance ( DuupX ('S n) (l ++ '[r0]) r a s
         , DipX n l (r0 & a & r ++ (a & s)) (a & r0 & r ++ (a & s))
         , (l ++ '[r0] ++ r ++ (a & s)) ~ (l ++ (r0 & r) ++ (a & s))
         , (l ++ '[a] ++ (r0 & r) ++ (a & s)) ~
           (l ++ (a & r0 & r ++ (a & s)))
         , (l ++ '[r0] ++ '[a] ++ r ++ (a & s)) ~
           (l ++ (r0 & a & r ++ (a & s)))
         ) => DuupX n l (r0 & r) a s where
  duupXImpl = duupXImpl @('S n) @(l ++ '[r0]) @r @a @s #
              dipXImpl @n @l @(r0 & a & r ++ (a & s)) @(a & r0 & r ++ (a & s)) swap

-- | @DUU+P@ macro. For example, `duupX @3` is `DUUUP`.
duupX
  :: forall (n :: GHC.Nat) inp out s a.
  ( DuupX 'Z '[] (Above (ToPeano (n - 1)) inp) a s
  , ((Above (ToPeano (n - 1)) inp) ++ (a & s)) ~ inp
  , (a & (Above (ToPeano (n - 1)) inp) ++ (a & s)) ~ out
  ) => inp :-> out
duupX = duupXImpl @'Z @'[] @(Above (ToPeano (n - 1)) inp) @a @s

papair :: a & b & c & s :-> ((a, b), c) & s
papair = pair # pair

ppaiir :: a & b & c & s :-> (a, (b, c)) & s
ppaiir = dip pair # pair

unpair :: (a, b) & s :-> a & b & s
unpair = dup # car # dip cdr

cdar :: (a1, (a2, b)) & s :-> a2 & s
cdar = cdr # car

cddr :: (a1, (a2, b)) & s :-> b & s
cddr = cdr # cdr

caar :: ((a, b1), b2) & s :-> a & s
caar = car # car

cadr :: ((a, b1), b2) & s :-> b1 & s
cadr = car # cdr

setCar :: (a, b1) & (b2 & s) :-> (b2, b1) & s
setCar = cdr # swap # pair

setCdr :: (a, b1) & (b2 & s) :-> (a, b2) & s
setCdr = car # pair

mapCar
  :: a & s :-> a1 & s
  -> (a, b) & s :-> (a1, b) & s
mapCar op = dup # cdr # dip (car # op) # swap # pair

mapCdr
  :: b & (a, b) & s :-> b1 & (a, b) & s
  -> (a, b) & s :-> (a, b1) & s
mapCdr op = dup # cdr # op # swap # car # pair

ifRight :: (b & s :-> s') -> (a & s :-> s') -> (Either a b & s :-> s')
ifRight l r = ifLeft r l

ifSome
  :: (a & s :-> s') -> (s :-> s') -> (Maybe a & s :-> s')
ifSome s n = ifNone n s

----------------------------------------------------------------------------
-- Additional Morley macros
----------------------------------------------------------------------------

-- | @view@ type synonym as described in A1.
data View (a :: Kind.Type) (r :: Kind.Type) = View
  { viewParam :: a
  , viewCallbackTo :: ContractAddr (a, Maybe r)
  } deriving stock Generic
    deriving anyclass IsoValue

view_ ::
     (KnownValue a, KnownValue r, NoOperation (a, r), NoBigMap (a, r))
  => (forall s0. (a, storage) & s0 :-> r : s0)
  -> View a r & storage & s :-> (List Operation, storage) & s
view_ code =
  coerce_ #
  unpair # dip (duupX @2) # dup # dip (pair # code # some) # pair # dip amount #
  transferTokens # nil # swap # cons # pair

-- | @void@ type synonym as described in A1.
data Void_ (a :: Kind.Type) (b :: Kind.Type) = Void_
  { voidParam :: a
  , voidProxy :: Lambda b b
  } deriving stock Generic
    deriving anyclass IsoValue

mkVoid :: a -> Void_ a b
mkVoid a = Void_ a nop

void_
  :: forall a b s s' anything.
      (KnownValue b)
  => a & s :-> b & s' -> Void_ a b & s :-> b & anything
void_ code =
  coerce_ @_ @(_, Lambda b b) #
  unpair # swap # dip code # swap # exec # failWith
