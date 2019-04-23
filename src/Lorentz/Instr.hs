module Lorentz.Instr
  ( ( # )
  , nop
  , drop
  , dup
  , swap
  , push
  , some
  , none
  , unit
  , ifNone
  , pair
  , car
  , cdr
  , left
  , right
  , ifLeft
  , nil
  , cons
  , size
  , emptySet
  , emptyMap
  , map
  , iter
  , mem
  , get
  , update
  , if_
  , ifCons
  , loop
  , loopLeft
  , lambda
  , exec
  , dip
  , failWith
  , cast
  , pack
  , unpack
  , concat
  , concat'
  , slice, isNat, add, sub, rsub, mul, ediv, abs
  , neg
  , lsl
  , lsr
  , or
  , and
  , xor
  , not
  , compare
  , eq0
  , neq0
  , lt0
  , gt0
  , le0
  , ge0
  , int
  , self
  , contract
  , transferTokens
  , setDelegate
  , createAccount
  , createContract
  , implicitAccount
  , now
  , amount
  , balance
  , checkSignature
  , sha256
  , sha512
  , blake2B
  , hashKey
  , stepsToQuota
  , source
  , sender
  , address
  ) where

import Prelude hiding
  (EQ, GT, LT, abs, and, compare, concat, drop, get, map, not, or, some, swap, xor)

import Data.Singletons (SingI)

import Lorentz.Type
import Lorentz.Value
import Michelson.Typed ((:+>), HasNoOp, Instr(..), Notes(NStar), ( # ))
import Michelson.Typed.Arith
import Michelson.Typed.Polymorphic

nop :: s :+> s
nop = Nop

drop :: a & s :+> s
drop = DROP

dup  :: a & s :+> a & a & s
dup = DUP

swap :: a & b & s :+> b & a & s
swap = SWAP

-- Want to do something like
--
-- push :: ToVal a => a -> Proxy t -> s :+> (t ': s)
-- push = PUSH . toVal
--
-- to automatically convert Haskell types to Vals

push :: forall t s .(SingI t, HasNoOp t) => Value t -> (s :+> t & s)
push = PUSH

some :: a & s :+> TOption a & s
some = SOME

none :: forall a s . SingI a => s :+> (TOption a & s)
none = NONE

unit :: s :+> TUnit & s
unit = UNIT

ifNone
  :: (s :+> s') -> (a & s :+> s') -> (TOption a & s :+> s')
ifNone = IF_NONE

pair :: a & b & s :+> TPair a b & s
pair = PAIR

car :: TPair a b & s :+> a & s
car = CAR

cdr :: TPair a b & s :+> b & s
cdr = CDR

left :: forall a b s. SingI b => a & s :+> TOr a b & s
left = LEFT

right :: forall a b s. SingI a => b & s :+> TOr a b & s
right = right

ifLeft
  :: (a & s :+> s') -> (b & s :+> s') -> (TOr a b & s :+> s')
ifLeft = IF_LEFT

nil :: SingI p => s :+> TList p & s
nil = NIL

cons :: a & TList a & s :+> TList a & s
cons = CONS

ifCons
  :: (a & TList a & s :+> s') -> (s :+> s') -> (TList a & s :+> s')
ifCons = IF_CONS

size :: SizeOp c => c & s :+> TNat & s
size = SIZE

emptySet :: (Typeable e, SingI e) => s :+> TSet e & s
emptySet = EMPTY_SET

emptyMap :: (Typeable k, Typeable v, SingI k, SingI v) => s :+> TMap k v & s
emptyMap = EMPTY_MAP

map
  :: (Typeable (MapOpInp c ': s), MapOp c)
  => (MapOpInp c & s :+> b & s) -> (c & s :+> MapOpRes c b & s)
map = MAP

iter
  :: (Typeable (IterOpEl c ': s), IterOp c)
  => (IterOpEl c & s :+> s) -> (c & s :+> s)
iter = ITER

mem :: MemOp c => Tc (MemOpKey c) & c & s :+> TBool & s
mem = MEM

get :: GetOp c => Tc (GetOpKey c) & c & s :+> TOption (GetOpVal c) & s
get = GET

update :: UpdOp c => Tc (UpdOpKey c) & UpdOpParams c & c & s :+> c & s
update = UPDATE

if_ :: (s :+> s') -> (s :+> s') -> (TBool & s :+> s')
if_ = IF

loop :: (s :+> TBool & s) -> (TBool & s :+> s)
loop = LOOP

loopLeft
  :: (a & s :+> TOr a b & s) -> (TOr a b & s :+> b & s)
loopLeft = LOOP_LEFT

lambda
  :: (Each [Typeable, SingI] [i, o])
  => Value (TLambda i o) -> (s :+> TLambda i o & s)
lambda = LAMBDA

exec :: a & TLambda a b & s :+> b & s
exec = EXEC

dip :: (s :+> s') -> (a & s :+> a & s')
dip = DIP

failWith :: (Typeable a, SingI a) => a & s :+> t
failWith = FAILWITH

cast :: SingI a => (a & s :+> a & s)
cast = CAST

pack :: (SingI a, HasNoOp a) => a & s :+> TBytes & s
pack = PACK

unpack :: (SingI a, HasNoOp a) => TBytes & s :+> TOption a & s
unpack = UNPACK

concat :: ConcatOp c => c & c & s :+> c & s
concat = CONCAT

concat' :: ConcatOp c => TList c & s :+> c & s
concat' = CONCAT'

slice :: SliceOp c => TNat & TNat & c & s :+> TOption c & s
slice = SLICE

isNat :: TInt & s :+> TOption TNat & s
isNat = ISNAT

add
  :: (ArithOp Add n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes Add n m) & s
add = ADD

sub
  :: (ArithOp Sub n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes Sub n m) & s
sub = SUB

rsub
  :: (ArithOp Sub n m, Typeable n, Typeable m)
  => Tc m & Tc n & s :+> Tc (ArithRes Sub n m) & s
rsub = SWAP # SUB

mul
  :: (ArithOp Mul n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes Mul n m) & s
mul = MUL

ediv :: EDivOp n m
     => Tc n & Tc m & s
     :+> TOption (TPair (Tc (EDivOpRes n m)) (Tc (EModOpRes n m))) & s
ediv = EDIV

abs :: UnaryArithOp Abs n => Tc n & s :+> Tc (UnaryArithRes Abs n) & s
abs = ABS

neg :: UnaryArithOp Neg n => Tc n & s :+> Tc (UnaryArithRes Neg n) & s
neg = NEG


lsl
  :: (ArithOp Lsl n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes Lsl n m) & s
lsl = LSL

lsr
  :: (ArithOp Lsr n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes Lsr n m) & s
lsr = LSR

or
  :: (ArithOp Or n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes Or n m) & s
or = OR

and
  :: (ArithOp And n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes And n m) & s
and = AND

xor
  :: (ArithOp Xor n m, Typeable n, Typeable m)
  => Tc n & Tc m & s :+> Tc (ArithRes Xor n m) & s
xor = XOR

not :: UnaryArithOp Not n => Tc n & s :+> Tc (UnaryArithRes Not n) & s
not = NOT

compare :: (ArithOp Compare n m, Typeable n, Typeable m)
        => Tc n & Tc m & s :+> Tc (ArithRes Compare n m) & s
compare = COMPARE

eq0 :: UnaryArithOp Eq' n => Tc n & s :+> Tc (UnaryArithRes Eq' n) & s
eq0 = EQ

neq0 :: UnaryArithOp Neq n => Tc n & s :+> Tc (UnaryArithRes Neq n) & s
neq0 = NEQ

lt0 :: UnaryArithOp Lt n => Tc n & s :+> Tc (UnaryArithRes Lt n) & s
lt0 = LT

gt0 :: UnaryArithOp Gt n => Tc n & s :+> Tc (UnaryArithRes Gt n) & s
gt0 = GT

le0 :: UnaryArithOp Le n => Tc n & s :+> Tc (UnaryArithRes Le n) & s
le0 = LE

ge0 :: UnaryArithOp Ge n => Tc n & s :+> Tc (UnaryArithRes Ge n) & s
ge0 = GE

int :: TNat & s :+> TInt & s
int = INT

self :: forall (cp :: T) s . s :+> TContract cp & s
self = SELF

contract :: (SingI p, Typeable p) => TAddress & s :+> TOption (TContract p) & s
contract = CONTRACT NStar

transferTokens
  :: (Typeable p, SingI p, HasNoOp p)
  => p & TMutez & TContract p & s :+> TOperation & s
transferTokens = TRANSFER_TOKENS

setDelegate :: TOption TKeyHash & s :+> TOperation & s
setDelegate = SET_DELEGATE

createAccount :: TKeyHash & TOption TKeyHash & TBool & TMutez & s
              :+> TOperation & TAddress & s
createAccount = CREATE_ACCOUNT


createContract :: (Each [Typeable, SingI, HasNoOp] [p, g])
               => '[ TPair p g ] :+> '[ TPair (TList TOperation) g ]
               -> TKeyHash & TOption TKeyHash & TBool & TBool & TMutez & g & s
               :+> TOperation & TAddress & s
createContract = CREATE_CONTRACT

implicitAccount :: TKeyHash & s :+> TContract TUnit & s
implicitAccount = IMPLICIT_ACCOUNT

now :: s :+> TTimestamp & s
now = NOW

amount :: s :+> TMutez & s
amount = AMOUNT

balance :: s :+> TMutez & s
balance = BALANCE

checkSignature :: TKey & TSignature & TBytes & s :+> TBool & s
checkSignature = CHECK_SIGNATURE

sha256 :: TBytes & s :+> TBytes & s
sha256 = SHA256

sha512 :: TBytes & s :+> TBytes & s
sha512 = SHA512

blake2B :: TBytes & s :+> TBytes & s
blake2B = BLAKE2B

hashKey :: TKey & s :+> TKeyHash & s
hashKey = HASH_KEY

stepsToQuota :: s :+> TNat & s
stepsToQuota = STEPS_TO_QUOTA

source :: s :+> TAddress & s
source = SOURCE

sender :: s :+> TAddress & s
sender = SENDER

address :: TContract a & s :+> TAddress & s
address = ADDRESS
