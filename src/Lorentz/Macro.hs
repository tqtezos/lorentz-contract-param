{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

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
  -- |
  -- They differ from the same macros in Michelson, because those
  -- macros use FAIL macro which is not informative (fails with unit).
  -- If you __really__ want Michelson versions (maybe to produce exact
  -- copy of an existing contract), you can pass 'UnspecifiedError', then
  -- FAILWITH will be called with unit.
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
  , assertUsing

  -- * Syntactic Conveniences
  , dipX
  , dropX
  , cloneX
  , duupX
  , elevateX
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
  , setInsert
  , mapInsert
  , setInsertNew
  , mapInsertNew
  , deleteMap
  , setDelete

  -- * Additional Morley macros
  , View (..)
  , Void_ (..)
  , VoidResult(..)
  , view_
  , void_
  , mkVoid
  ) where

import Prelude hiding (compare, drop, some, swap)

import qualified Data.Kind as Kind
import Data.Singletons (SingI)
import Data.Vinyl.TypeLevel (Nat(..))
import GHC.TypeNats (type (+), type (-))
import qualified GHC.TypeNats as GHC (Nat)

import Lorentz.Arith
import Lorentz.Doc
import Lorentz.Base
import Lorentz.Coercions
import Lorentz.Constraints
import Lorentz.Errors
import Lorentz.Instr
import Lorentz.Value
import Michelson.Typed.Arith
import Michelson.Typed.Haskell.Value
import Util.Peano

----------------------------------------------------------------------------
-- Compare
----------------------------------------------------------------------------

eq :: (ArithOpHs Compare n n, UnaryArithOpHs Eq' (ArithResHs Compare n n))
   => n & n & s :-> (UnaryArithResHs Eq' (ArithResHs Compare n n)) & s
eq = compare # eq0

neq :: (ArithOpHs Compare n n, UnaryArithOpHs Neq (ArithResHs Compare n n))
   => n & n & s :-> (UnaryArithResHs Neq (ArithResHs Compare n n)) & s
neq = compare # neq0

gt :: (ArithOpHs Compare n n, UnaryArithOpHs Gt (ArithResHs Compare n n))
   => n & n & s :-> (UnaryArithResHs Gt (ArithResHs Compare n n)) & s
gt = compare # gt0

le :: (ArithOpHs Compare n n, UnaryArithOpHs Le (ArithResHs Compare n n))
   => n & n & s :-> (UnaryArithResHs Le (ArithResHs Compare n n)) & s
le = compare # le0

ge :: (ArithOpHs Compare n n, UnaryArithOpHs Ge (ArithResHs Compare n n))
   => n & n & s :-> (UnaryArithResHs Ge (ArithResHs Compare n n)) & s
ge = compare # ge0

lt :: (ArithOpHs Compare n n, UnaryArithOpHs Lt (ArithResHs Compare n n))
   => n & n & s :-> (UnaryArithResHs Lt (ArithResHs Compare n n)) & s
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

type IfCmpXConstraints a op =
  (Typeable a, ArithOpHs Compare a a
  , UnaryArithOpHs op (ArithResHs Compare a a)
  , UnaryArithResHs op (ArithResHs Compare a a) ~ Bool)

ifEq
  :: (IfCmpXConstraints a Eq')
  => (s :-> s') -> (s :-> s') -> (a & a & s :-> s')
ifEq l r = eq # if_ l r

ifNeq
  :: (IfCmpXConstraints a Neq)
  => (s :-> s') -> (s :-> s') -> (a & a & s :-> s')
ifNeq l r = neq # if_ l r

ifLt
  :: (IfCmpXConstraints a Lt)
  => (s :-> s') -> (s :-> s') -> (a & a & s :-> s')
ifLt l r = lt # if_ l r

ifGt
  :: (IfCmpXConstraints a Gt)
  => (s :-> s') -> (s :-> s') -> (a & a & s :-> s')
ifGt l r = gt # if_ l r

ifLe
  :: (IfCmpXConstraints a Le)
  => (s :-> s') -> (s :-> s') -> (a & a & s :-> s')
ifLe l r = le # if_ l r

ifGe
  :: (IfCmpXConstraints a Ge)
  => (s :-> s') -> (s :-> s') -> (a & a & s :-> s')
ifGe l r = ge # if_ l r

----------------------------------------------------------------------------
-- Fail
----------------------------------------------------------------------------

-- | Analog of the FAIL macro in Michelson. Its usage is discouraged
-- because it doesn't carry any information about failure.
{-# WARNING fail_ "'fail_' remains in code" #-}
fail_ :: a :-> c
fail_ = unit # failWith

----------------------------------------------------------------------------
-- Assertions
----------------------------------------------------------------------------

assert :: IsError err => err -> Bool & s :-> s
assert reason = if_ nop (failUsing reason)

assertEq0 :: (IfCmp0Constraints a Eq', IsError err) => err -> a & s :-> s
assertEq0 reason = ifEq0 nop (failUsing reason)

assertNeq0 :: (IfCmp0Constraints a Neq, IsError err) => err -> a & s :-> s
assertNeq0 reason = ifNeq0 nop (failUsing reason)

assertLt0 :: (IfCmp0Constraints a Lt, IsError err) => err -> a & s :-> s
assertLt0 reason = ifLt0 nop (failUsing reason)

assertGt0 :: (IfCmp0Constraints a Gt, IsError err) => err -> a & s :-> s
assertGt0 reason = ifGt0 nop (failUsing reason)

assertLe0 :: (IfCmp0Constraints a Le, IsError err) => err -> a & s :-> s
assertLe0 reason = ifLe0 nop (failUsing reason)

assertGe0 :: (IfCmp0Constraints a Ge, IsError err) => err -> a & s :-> s
assertGe0 reason = ifGe0 nop (failUsing reason)

assertEq :: (IfCmpXConstraints a Eq', IsError err) => err -> a & a & s :-> s
assertEq reason = ifEq nop (failUsing reason)

assertNeq :: (IfCmpXConstraints a Neq, IsError err) => err -> a & a & s :-> s
assertNeq reason = ifNeq nop (failUsing reason)

assertLt :: (IfCmpXConstraints a Lt, IsError err) => err -> a & a & s :-> s
assertLt reason = ifLt nop (failUsing reason)

assertGt :: (IfCmpXConstraints a Gt, IsError err) => err -> a & a & s :-> s
assertGt reason = ifGt nop (failUsing reason)

assertLe :: (IfCmpXConstraints a Le, IsError err) => err -> a & a & s :-> s
assertLe reason = ifLe nop (failUsing reason)

assertGe :: (IfCmpXConstraints a Ge, IsError err) => err -> a & a & s :-> s
assertGe reason = ifGe nop (failUsing reason)

assertNone :: IsError err => err -> Maybe a & s :-> s
assertNone reason = ifNone nop (failUsing reason)

assertSome :: IsError err => err -> Maybe a & s :-> a & s
assertSome reason = ifNone (failUsing reason) nop

assertLeft :: IsError err => err -> Either a b & s :-> a & s
assertLeft reason = ifLeft nop (failUsing reason)

assertRight :: IsError err => err -> Either a b & s :-> b & s
assertRight reason = ifLeft (failUsing reason) nop

assertUsing
  :: IsError a
  => a -> Bool & s :-> s
assertUsing err = if_ nop $ failUsing err

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

-- | Custom Lorentz macro that drops element with given index
-- (starting from 0) from the stack.
dropX
  :: forall (n :: GHC.Nat) a inp out s s'.
  ( DipX (ToPeano n) (Above (ToPeano n) inp) s s'
  , ((Above (ToPeano n) inp) ++ s) ~ inp
  , ((Above (ToPeano n) inp) ++ s') ~ out
  , s ~ (a ': s')
  )
  => inp :-> out
dropX = dipX @n @inp @out @s @s' drop

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

class DuupX (n :: Peano) (s :: [Kind.Type]) (a :: Kind.Type) where
  duupXImpl :: s :-> a & s

instance DuupX ('S 'Z) (a & xs) a where
  duupXImpl = dup

instance DuupX ('S n) xs a => DuupX ('S ('S n)) (x & xs) a where
  duupXImpl = dip (duupXImpl @('S n) @xs @a) # swap

-- | @DUU+P@ macro. For example, `duupX @3` is `DUUUP`, it puts
-- the 3-rd (starting from 1) element to the top of the stack.
duupX
  :: forall (n :: GHC.Nat) inp .
  ( DuupX (ToPeano n) inp (At (ToPeano (n - 1)) inp)
  )
  => inp :-> (At (ToPeano (n - 1)) inp) & inp
duupX = duupXImpl @(ToPeano n) @inp @(At (ToPeano (n - 1)) inp)

-- | Move item with given index (starting from 0) to the top of the stack.
--
-- TODO: probably it can be implemented more efficiently, so if we
-- ever want to optimize gas consumption we can rewrite it.
-- It only makes sense if it's applied to a relatively large index.
elevateX
  :: forall (n :: GHC.Nat) inp out s a.
  ( DuupX (ToPeano (1 + n)) inp a
  , DipX (ToPeano (n + 1)) (Above (ToPeano (n + 1)) (a : inp)) (a : s) s
  , (a ': (Above (ToPeano n) inp) ++ s) ~ out
  , a ~ At (ToPeano n) inp

  -- These 3 constraints must hold if the above holds, but GHC can't deduce it.
  -- Btw, last constraint must always hold.
  , (Above (ToPeano (n + 1)) (a ': inp) ++ (a ': s)) ~ (a ': inp)
  , (Above (ToPeano (n + 1)) (a : inp) ++ s) ~ (a : (Above (ToPeano n) inp ++ s))
  , ((1 + n) - 1) ~ n
  ) => inp :-> out
elevateX = duupX @(1 + n) @inp # dropX @(n + 1) @a @(a ': inp) @out @(a ': s) @s

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

-- | Various convenient instructions on maps.
class MapInstrs map where
  -- | Specialized version of 'update'.
  mapUpdate :: IsComparable k => k : Maybe v : map k v : s :-> map k v : s

  -- | Insert given element into map.
  mapInsert :: IsComparable k => k : v : map k v : s :-> map k v : s
  mapInsert = dip some # mapUpdate

  -- | Insert given element into map, ensuring that it does not overwrite
  -- any existing entry.
  --
  -- As first argument accepts container name (for error message).
  mapInsertNew
    :: (IsComparable k, KnownValue e)
    => (forall s0. k : s0 :-> e : s0)
    -> k : v : map k v : s :-> map k v : s

  -- | Delete element from the map.
  deleteMap
    :: forall k v s. (IsComparable k, KnownValue k, KnownValue v)
    => k : map k v : s :-> map k v : s
  deleteMap = dip (none @v) # mapUpdate

instance MapInstrs Map where
  mapUpdate = update
  mapInsertNew mkErr = failingWhenPresent mkErr # mapInsert
instance MapInstrs BigMap where
  mapUpdate = update
  mapInsertNew mkErr = failingWhenPresent mkErr # mapInsert

-- | Insert given element into set.
--
-- This is a separate function from 'updateMap' because stacks they operate with
-- differ in length.
setInsert :: IsComparable e => e & Set e & s :-> Set e & s
setInsert = dip (push True) # update

-- | Insert given element into set, ensuring that it does not overwrite
-- any existing entry.
--
-- As first argument accepts container name.
setInsertNew
  :: (IsComparable e, KnownValue err)
  => (forall s0. e : s0 :-> err : s0)
  -> e & Set e & s :-> Set e & s
setInsertNew desc = dip (push True) # failingWhenPresent desc # update

-- | Delete given element from the set.
setDelete :: IsComparable e => e & Set e & s :-> Set e & s
setDelete = dip (push False) # update

----------------------------------------------------------------------------
-- Additional Morley macros
----------------------------------------------------------------------------

-- | @view@ type synonym as described in A1.
data View (a :: Kind.Type) (r :: Kind.Type) = View
  { viewParam :: a
  , viewCallbackTo :: ContractAddr r
  } deriving stock Generic
    deriving anyclass IsoValue

instance Each [Typeable, TypeHasDoc] [a, r] => TypeHasDoc (View a r) where
  typeDocMdDescription =
    "`View a r` accepts an argument of type `a` and callback contract \
    \which accepts `r` and returns result via calling that contract.\n\
    \Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/blob/master/A/A1.md#view-entry-points)."
  typeDocMdReference = poly2TypeDocMdReference
  typeDocDependencies p =
    genericTypeDocDependencies p <>
    [SomeTypeWithDoc (Proxy @()), SomeTypeWithDoc (Proxy @Integer)]
  typeDocHaskellRep =
    haskellRepNoFields $ concreteTypeDocHaskellRep @(View () Integer)
  typeDocMichelsonRep =
    concreteTypeDocMichelsonRep @(View () Integer)

view_ ::
     (KnownValue r, NoOperation r, NoBigMap r)
  => (forall s0. (a, storage) & s0 :-> r : s0)
  -> View a r & storage & s :-> (List Operation, storage) & s
view_ code =
  coerce_ #
  unpair # dip (duupX @2) # pair # code # dip amount #
  transferTokens # nil # swap # cons # pair

-- | @void@ type synonym as described in A1.
data Void_ (a :: Kind.Type) (b :: Kind.Type) = Void_
  { voidParam :: a
    -- ^ Entry point argument.
  , voidResProxy :: Lambda b b
    -- ^ Type of result reported via 'failWith'.
  } deriving stock Generic
    deriving anyclass IsoValue

instance Each [Typeable, TypeHasDoc] [a, r] => TypeHasDoc (Void_ a r) where
  typeDocName _ = "Void"
  typeDocMdDescription =
    "`Void a r` accepts an argument of type `a` and returns a value of type `r` \
    \via `FAILWITH` instruction.\n\
    \Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/blob/master/A/A1.md#void-entry-points)."
  typeDocMdReference tp =
    -- Avoiding trailing underscore
    customTypeDocMdReference
      ("Void", DType tp)
      [ DType (Proxy @a)
      , DType (Proxy @r)
      ]
  typeDocDependencies p =
    genericTypeDocDependencies p <>
    [SomeTypeWithDoc (Proxy @()), SomeTypeWithDoc (Proxy @Integer)]
  typeDocHaskellRep p = do
    (_, rhs) <- haskellRepNoFields (concreteTypeDocHaskellRep @(Void_ () Integer)) p
    return (Just "Void () Integer", rhs)
  typeDocMichelsonRep p =
    let (_, rhs) = concreteTypeDocMichelsonRep @(Void_ () Integer) p
    in (Just "Void () Integer", rhs)

-- | Newtype over void result type used in tests to
-- distinguish successful void result from other errors.
--
-- Usage example:
-- lExpectFailWith (== VoidResult roleMaster)`
newtype VoidResult r = VoidResult { unVoidResult :: r }
  deriving stock (Generic)
  deriving newtype (Eq)

instance ( Typeable r, IsoValue r
         , Each [Typeable, SingI] '[ToT r]
         ) =>
         IsError (VoidResult r) where
  errorToVal = customErrorToVal
  errorFromVal = customErrorFromVal

instance CustomErrorNoIsoValue (VoidResult r) => IsoValue (VoidResult r) where
  type ToT (VoidResult r) = CustomErrorNoIsoValue (VoidResult r)
  toVal = error "impossible"
  fromVal = error "impossible"

mkVoid :: forall b a. a -> Void_ a b
mkVoid a = Void_ a nop

void_
  :: forall a b s s' anything.
      (Typeable b, IsoValue b, KnownValue b)
  => a & s :-> b & s' -> Void_ a b & s :-> anything
void_ code =
  coerce_ @_ @(_, Lambda b b) #
  unpair # swap # dip code # swap # exec # failUsingArg @(VoidResult b) #cVoidResult
