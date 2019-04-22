-- | Common Michelson macros defined using Lorentz syntax.
module Lorentz.Macro
  ( -- * Compare
    eq
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
  , dupX
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
  ) where

import Prelude hiding (compare, swap)

import Data.Vinyl.TypeLevel (Nat(..))
import GHC.TypeNats (type (-))
import qualified GHC.TypeNats as GHC (Nat)

import Lorentz.Instr
import Lorentz.Type
import Michelson.Typed ((:+>), ( # ))
import Michelson.Typed.Arith
import Util.Peano

----------------------------------------------------------------------------
-- Compare
----------------------------------------------------------------------------

eq :: (ArithOp Compare n m, UnaryArithOp Eq' (ArithRes Compare n m), Typeable n, Typeable m)
   => Tc n & Tc m & s :+> Tc (UnaryArithRes Eq' (ArithRes Compare n m)) & s
eq = compare # eq0

neq :: (ArithOp Compare n m, UnaryArithOp Neq (ArithRes Compare n m), Typeable n, Typeable m)
   => Tc n & Tc m & s :+> Tc (UnaryArithRes Neq (ArithRes Compare n m)) & s
neq = compare # neq0

gt :: (ArithOp Compare n m, UnaryArithOp Gt (ArithRes Compare n m), Typeable n, Typeable m)
   => Tc n & Tc m & s :+> Tc (UnaryArithRes Gt (ArithRes Compare n m)) & s
gt = compare # gt0

le :: (ArithOp Compare n m, UnaryArithOp Le (ArithRes Compare n m), Typeable n, Typeable m)
   => Tc n & Tc m & s :+> Tc (UnaryArithRes Le (ArithRes Compare n m)) & s
le = compare # le0

ge :: (ArithOp Compare n m, UnaryArithOp Ge (ArithRes Compare n m), Typeable n, Typeable m)
   => Tc n & Tc m & s :+> Tc (UnaryArithRes Ge (ArithRes Compare n m)) & s
ge = compare # ge0

lt :: (ArithOp Compare n m, UnaryArithOp Lt (ArithRes Compare n m), Typeable n, Typeable m)
   => Tc n & Tc m & s :+> Tc (UnaryArithRes Lt (ArithRes Compare n m)) & s
lt = compare # lt0

type IfCmp0Constraints a op =
  (UnaryArithOp op a, (UnaryArithRes op a ~ CBool))

ifEq0
  :: (IfCmp0Constraints a Eq')
  => (s :+> s') -> (s :+> s') -> (Tc a & s :+> s')
ifEq0 l r = eq0 # if_ l r

ifNeq0
  :: (IfCmp0Constraints a Neq)
  => (s :+> s') -> (s :+> s') -> (Tc a & s :+> s')
ifNeq0 l r = neq0 # if_ l r

ifLt0
  :: (IfCmp0Constraints a Lt)
  => (s :+> s') -> (s :+> s') -> (Tc a & s :+> s')
ifLt0 l r = lt0 # if_ l r

ifGt0
  :: (IfCmp0Constraints a Gt)
  => (s :+> s') -> (s :+> s') -> (Tc a & s :+> s')
ifGt0 l r = gt0 # if_ l r

ifLe0
  :: (IfCmp0Constraints a Le)
  => (s :+> s') -> (s :+> s') -> (Tc a & s :+> s')
ifLe0 l r = le0 # if_ l r

ifGe0
  :: (IfCmp0Constraints a Ge)
  => (s :+> s') -> (s :+> s') -> (Tc a & s :+> s')
ifGe0 l r = ge0 # if_ l r

type IfCmpXConstraints a b op =
  (Typeable a, Typeable b, ArithOp Compare a b
  , UnaryArithOp op (ArithRes Compare a b)
  , UnaryArithRes op (ArithRes Compare a b) ~ CBool)

ifEq
  :: (IfCmpXConstraints a b Eq')
  => (s :+> s') -> (s :+> s') -> (Tc a & Tc b & s :+> s')
ifEq l r = eq # if_ l r

ifNeq
  :: (IfCmpXConstraints a b Neq)
  => (s :+> s') -> (s :+> s') -> (Tc a & Tc b & s :+> s')
ifNeq l r = neq # if_ l r

ifLt
  :: (IfCmpXConstraints a b Lt)
  => (s :+> s') -> (s :+> s') -> (Tc a & Tc b & s :+> s')
ifLt l r = lt # if_ l r

ifGt
  :: (IfCmpXConstraints a b Gt)
  => (s :+> s') -> (s :+> s') -> (Tc a & Tc b & s :+> s')
ifGt l r = gt # if_ l r

ifLe
  :: (IfCmpXConstraints a b Le)
  => (s :+> s') -> (s :+> s') -> (Tc a & Tc b & s :+> s')
ifLe l r = le # if_ l r

ifGe
  :: (IfCmpXConstraints a b Ge)
  => (s :+> s') -> (s :+> s') -> (Tc a & Tc b & s :+> s')
ifGe l r = ge # if_ l r

----------------------------------------------------------------------------
-- Fail
----------------------------------------------------------------------------

fail_ :: a :+> c
fail_ = unit # failWith

----------------------------------------------------------------------------
-- Assertions
----------------------------------------------------------------------------

assert :: TBool & s :+> s
assert = if_ nop fail_

assertEq0 :: IfCmp0Constraints a Eq' => Tc a & s :+> s
assertEq0 = ifEq0 nop fail_

assertNeq0 :: IfCmp0Constraints a Neq => Tc a & s :+> s
assertNeq0 = ifNeq0 nop fail_

assertLt0 :: IfCmp0Constraints a Lt => Tc a & s :+> s
assertLt0 = ifLt0 nop fail_

assertGt0 :: IfCmp0Constraints a Gt => Tc a & s :+> s
assertGt0 = ifGt0 nop fail_

assertLe0 :: IfCmp0Constraints a Le => Tc a & s :+> s
assertLe0 = ifLe0 nop fail_

assertGe0 :: IfCmp0Constraints a Ge => Tc a & s :+> s
assertGe0 = ifGe0 nop fail_

assertEq :: IfCmpXConstraints a b Eq' => Tc a & Tc b & s :+> s
assertEq = ifEq nop fail_

assertNeq :: IfCmpXConstraints a b Neq => Tc a & Tc b & s :+> s
assertNeq = ifNeq nop fail_

assertLt :: IfCmpXConstraints a b Lt => Tc a & Tc b & s :+> s
assertLt = ifLt nop fail_

assertGt :: IfCmpXConstraints a b Gt => Tc a & Tc b & s :+> s
assertGt = ifGt nop fail_

assertLe :: IfCmpXConstraints a b Le => Tc a & Tc b & s :+> s
assertLe = ifLe nop fail_

assertGe :: IfCmpXConstraints a b Ge => Tc a & Tc b & s :+> s
assertGe = ifGe nop fail_

assertNone :: TOption a & s :+> s
assertNone = ifNone nop fail_

assertSome :: TOption a & s :+> a & s
assertSome = ifNone fail_ nop

assertLeft :: TOr a b & s :+> a & s
assertLeft = ifLeft nop fail_

assertRight :: TOr a b & s :+> b & s
assertRight = ifLeft fail_ nop

----------------------------------------------------------------------------
-- Syntactic Conveniences
----------------------------------------------------------------------------

type family Above (n :: Peano) s where
  Above  'Z _s = '[]
  Above ('S n) (a ': s) = (a ': Above n s)

class DipX (n :: Peano) (head :: [T]) (s :: [T]) (s' :: [T]) where
  dipXImpl :: s :+> s' -> head ++ s :+> head ++ s'
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
  => (s :+> s') -> inp :+> out
dipX = dipXImpl @(ToPeano n) @(Above (ToPeano n) inp)

class DupX (n :: Peano) a s where
  type DupXT n a s:: [T]
  dupXImpl :: a & s :+> DupXT n a s
instance DupX 'Z a s where
  type DupXT 'Z a s = a & s
  dupXImpl = nop
instance (DupX n a s) => DupX ('S n) a s where
  type DupXT ('S n) a s = a ': DupXT n a s
  dupXImpl = dup # dip (dupXImpl @n)

-- | Duplicate the top of the stack @n@ times.
--
-- For example, `dupX @3` has type `a & s :+> a & a & a & a & s`.
dupX
  :: forall (n :: GHC.Nat) a s. DupX (ToPeano n) a s
  => a & s :+> DupXT (ToPeano n) a s
dupX = dupXImpl @(ToPeano n)

class DuupX (n :: Peano) (l :: [T]) (r :: [T]) (a :: T) (s :: [T]) where
  duupXImpl :: l ++ r ++ (a & s) :+> l ++ '[a] ++ r ++ (a & s)
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
  ) => inp :+> out
duupX = duupXImpl @'Z @'[] @(Above (ToPeano (n - 1)) inp) @a @s

papair :: a & b & c & s :+> TPair (TPair a b) c & s
papair = pair # pair

ppaiir :: a & b & c & s :+> TPair a (TPair b c) & s
ppaiir = dip pair # pair

unpair :: TPair a b & s :+> a & b & s
unpair = dup # car # dip cdr

cdar :: TPair a1 (TPair a2 b) & s :+> a2 & s
cdar = cdr # car

cddr :: TPair a1 (TPair a2 b) & s :+> b & s
cddr = cdr # cdr

caar :: TPair (TPair a b1) b2 & s :+> a & s
caar = car # car

cadr :: TPair (TPair a b1) b2 & s :+> b1 & s
cadr = car # cdr

setCar :: TPair a b1 & (b2 & s) :+> TPair b2 b1 & s
setCar = cdr # swap # pair

setCdr :: TPair a b1 & (b2 & s) :+> TPair a b2 & s
setCdr = car # pair

mapCar
  :: a & s :+> a1 & s
  -> TPair a b & s :+> TPair a1 b & s
mapCar op = dup # cdr # dip (car # op) # swap # pair

mapCdr
  :: b & TPair a b & s :+> b1 & TPair a b & s
  -> TPair a b & s :+> TPair a b1 & s
mapCdr op = dup # cdr # op # swap # car # pair

ifRight :: (b & s :+> s') -> (a & s :+> s') -> (TOr a b & s :+> s')
ifRight l r = ifLeft r l

ifSome
  :: (a & s :+> s') -> (s :+> s') -> (TOption a & s :+> s')
ifSome s n = ifNone n s
