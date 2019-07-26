# Indigo eDSL

Indigo eDSL is a high level language over Lorentz.
The main feature of the language is that it supports variables
and that you are freed of a burden of manual stack management.
You can construct expression using variables and constants within them
and they will be compiled in Lorentz.

Also the language supports all typical imperative statements like If, While, For, etc.
Variables are destroyed when you leave a scope, like in any other imperative languages.

## Overall idea
The main motivation to create this eDSL was a burden of manual stack management in Lorentz.

During writing contract code in Lorentz you have access to the typed stack
which is basically Haskell heterogeneous list. Repetitive pattern during this process
is storing bunch of values at the end of the stack and copying required ones on the top of
the stack when you have to operate with them. Sometimes a developer can
leverage the other tricks, but the described one seemed to be most common to me.

So, Indigo is just called up to handle this routine in the similar way, namely,
storing values at the end of the stack with ability to refer to them via variables.
Each variable has its own index and can be found in the stack using this index.

## IndigoM monad
The basic idea for Indigo is straightforward.
We just want to have a state monad that stores a list which corresponds
to the stack. Each element of the list is the index of a variable which
refers to this corresponding stack element.
Also, it would be nice to store number of variables which has been already allocated
so far, to be able to assign a next variable next index which hasn't been already used.

As we go with explanation, I can give some snippet of Haskell code.

```haskell
newtype Var = Var Word

data StkEl
  = NoRef
  | Ref Var

newtype IndigoM a = IndigoM (State ([StkEl], Word) a)
```

This code could probably work, it has two main disadvantages:
* It's not strong typed. We have strong typed Lorentz stack but this `IndigoM`
  doesn't ensure any of these type level invariants which Lorentz provides.
* It is not connected to the real Lorentz code at all.
  Strictly speaking we could return Lorentz code as result of an execution of this monad,
  and then glue result of two sequent results, but again, it's very fragile
  (from the perspective of types).

Let's bring more type safety here.

First of all, let's make `Var` typed:
```haskell
newtype Var a = Var Word
```
You can see that `Var` has a phantom type parameter which is the type
of the stack element where a variable refer to.

Then let's propagate this change to `StkEl`
```haskell
data StkEl a
  = NoRef
  | Typeable a => Ref (Var a)
```
(don't pay attention to the `Typeable a` constraint at this stage,
it's a minor detail and will be explained in the next chapter).

Now let's define heterogeneous list of `StkEl`

```haskell
type StackVars (stk :: [Kind.Type]) = Rec StkEl stk
```

So we got the following definition of `IndigoM`:
```haskell
newtype IndigoM stk a = IndigoM (State (StackVars stk, Word) a)
```

But now, if we try to connect two sequent expressions having `IndigoM` type via `>>=`,
we'll be obliged to provide expression with the same `stk`, however, the second
one actually has `stk` which depends on a result type of the first expression.

So, ideally, we would like to have such `IndigoM` which is akin `State` monad described
above and also has an input and an output stack types as its type parameters.
Something like this
```haskell
newtype IndigoM inp out a = ...
```
and then the bind operator would have a type like this:
```haskell
(>>=) :: IndigoM inp out a -> (a -> IndigoM out out1 b) -> IndigoM inp out1 b
```
so such bind operator would glue two expressions with matched types.
But the problem is that it's already not bind for ordinary `State`,
also this bind doesn't match to the bind from `Monad` type class.

Fortunatelly, there is such thing which called
[indexed monad](http://hackage.haskell.org/package/category-extras-0.53.1/docs/Control-Monad-Indexed-State.html#t:IxState).
Its definition looks like this:
```haskell
newtype IxState i j a = IxState { runIxState :: i -> (a, j) }
```
and it's basically `State` monad which returns state of a new type everytime.

Let's go closer to our case, and introduce substitue specific types to this definition
to build finall version of `IndigoM` monad:
```haskell
newtype IndigoM inp out a = IndigoM { runIndigoM :: (StackVars inp, Word) -> (StackVars out, Word, inp :-> out) }
```
Though, it may look scary but basically it's a state monad which consumes
the references on the stack and number of allocated variables and returns
new references, increased number of allocated variables and Lorentz code which is generated
in result of the execution.

You can get more details in this [module](src/Indigo/State.hs), where
`MetaData` is a consuming tuple and `GenCode` is a resulting one.
Also, in this module you can see definition of `>>=` operator which together with
`RebindableSyntax` extension can be used in `do` syntax, what make code really neat.

## Variables and expression evaluation
Now, when we got so solid strong typed construction let's try to examine it further
and describe some basic operations.

Let's try to come up with assignment operator:
function which would create a new variable from some expression.
Expression is as usual thing which can go on the right hand of assignment.
Let's define simple `Expr` datatype with three basic constructors:
```haskell
data Expr a where
  C   :: a -> Expr a
  V   :: Var a -> Expr a
  Add :: Expr Int -> Expr Int -> Expr Int
```
First of them create an expression from a constant, the second from variable
and the last one is for addition of two expressions of `Int` type.

Let's define a function which will compile `Expr` to Lorentz code which computes
value of passed expression and leave it on the top of the stack:
```haskell
compileExpr :: Expr a -> IndigoM inp (a & inp) ()
```
It'll have this type because in result of the execution we'll have
an expression of type `a` on the top of the stack.

Let's define it step by step, explaining definition for each constructor:
```haskell
compileExpr (C a) = do
  md <- iget
  iput $ GenCode (pushNoRefMd md) (push a)
```
First of all, notice that we use `do` syntax which comes from `RebindableSyntax` extension
for `IndigoM`.
In the first line of this function we just get current state (which has type `MetaData`),
in the second line first of all we construct `GenCode` datatype which is resulting type.
`GenCode` consists of `MetaData` and generated Lorentz code.
Generated code will be just `push` Lorentz instruction, `pushNoRefMd` hasn't been defined yet.
However, it just push `NoRef` of type `StkEl a` on the top of the stack
to satisfy output type of the stack `a & inp`.

The next step is `Add` constructor:
```haskell
compileExpr (Add e1 e2) = do
  compileExpr e2
  compileExpr e1
  IndigoM $ \md -> ((), GenCode (pushNoRefMd $ popNoRefMd $ popNoRefMd md) add)
```
First two lines are straightforward, we just call `compileExpr` recursively for operands of `Add`.
The last line looks a little bit trickier, but basically it drops two ints from the top and
take one as a result.

And the last and the most hardest step, implementation for a variable:
```haskell
compileExpr (V a) = do
  md@(MetaData s _) <- iget
  iput $ GenCode (pushNoRefMd md) (lookupVar a s)
```

Firstly, it's better to realise logic behind this code.
As was stated above, the main idea that we assign each variable an index
and mark each element of the stack with an index of a variable which refers to this element.
Now it's time to bind things together.
To compute an expression which is just a variable we have to generate Lorentz code which
make `duup` of the element corresponding to this variable.
This logic implements recursive function `lookupVar` which has the following type:

```haskell
lookupVar :: forall a inp . Typeable a => Var a -> StackVars inp -> inp :-> a & inp
```
This function iterates over `StackVars` and searches for a stack element with the same index as `Var` holds.
After it found such element it check that types of `a` and a stack element matches (using `Typeable` typeclass).
As result this function returns code of `duup` for needed stack element. This code has
a recursive structure:
* `g[i] = L.dip g[i-1] # L.swap`, where `g[i-1]` is code which is generated for the previous element of the stack.
* `g[0] = dup`, obviously, that if needed variable is on the top of the stack we just need to make `dup`.

You can get more details about `lookupVar` in this [module](src/Indigo/State.hs).
You can notice that actually `lookupVar` returns `VarActions`.
`VarActions` contains not only getter of a variable, also setter, code which assigns
to a variable value on the top of the stack, also field setter which comes into play
when a variable holds a value of composite type.

And the last step to write assignment operator is create a variable which
refers to the top of the stack where a computed expression takes place.
This can be done with function having type
```haskell
makeTopVar :: Typeable x => IndigoM (x & inp) (x & inp) (Var x)
```
Its implementation you can find in the same module as `lookupVar`.

As well in the module [Expr](src/Indigo/Expr.hs) you can find more usefull constructors of `Expr`
and their compilation to Lorentz.

## Imperative statements
The icing on the cake is imperative statements like `if`, `while`, `assert` and others
which can be used in code in Indigo.

Let's just take `if_` from [Language](src/Indigo/Language.hs) module and explain its semantic.
`if_` has the following type:
```haskell
if_ :: forall inp xs ys .
  ( DestroyPrefix xs inp
  , DestroyPrefix ys inp
  )
  => Expr Bool
  -> IndigoM inp xs ()
  -> IndigoM inp ys ()
  -> IndigoM inp inp ()
```
First of all it takes three arguments: an boolean expression and `Indigo` code for true and false branch.
Let's take a closer look at branches code and resulting one.
Branches can end up with different stack types, but we have to unify them somehow to fit in the same type.
This unification is obvious and comes from imperative languages, where there exist such notion
as "scope". If execution leaves a scope all variables defined within it are destroyed.
The same thing happens here. `DestroyPrefix` just ensures that `xs` and `ys` are both suffixies
of resulting stack type (which is `inp`), so prefixes of `xs` and `ys` can be freely dropped
(among with stack references).

Pay attention to the fact, that the reason that branches return `()` is a protection from
accidentally return of a variable which will refers to the non-existing stack elements which
are destroyed after execution leaves the scope. It can be rewritten to return `Expr a`
if it's really needed.

Also, notice that though `xs` and `ys` are suffixies of `inp` if branches directly operate
with internals of `IndigoM`, they can spoil the stack and remove some references.
Which will cause an error when you try to evaluate a variable which refers to non-existing element.
Because of that it makes sense not to export functions which allow to modify `IndigoM` state directly.

## Examples
You can find some examples of Indigo code at this [module](src/Indigo/Examples.hs).
