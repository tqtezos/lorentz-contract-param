-- | Functions to check whether two values are equal if their types
-- have parameters and it's not statically known whether they these
-- parameters have the same types.

module Michelson.EqParam
  ( eqParam1
  , eqParam2
  , eqParam3
  ) where

import Data.Typeable ((:~:)(..), eqT)

-- | Suppose you have a data type `X` with parameter `a` and you have
-- two values: `x1 :: X a1` and `x2 :: X a2`. You can't compare them
-- using '==', because they have different types. However, you can
-- compare them using 'eqParam1' as long as both parameters are
-- 'Typeable'.
eqParam1 ::
     forall a1 a2 t.
     ( Typeable a1
     , Typeable a2
     , Eq (t a1)
     )
  => t a1
  -> t a2
  -> Bool
eqParam1 t1 t2 = isJust @() $ do
  Refl <- eqT @a1 @a2
  guard (t1 == t2)

-- | Version of 'eqParam1' for types with 2 parameters.
eqParam2 ::
     forall a1 a2 b1 b2 t.
     ( Typeable a1
     , Typeable a2
     , Typeable b1
     , Typeable b2
     , Eq (t a1 b2)
     )
  => t a1 b1
  -> t a2 b2
  -> Bool
eqParam2 t1 t2 = isJust @() $ do
  Refl <- eqT @a1 @a2
  Refl <- eqT @b1 @b2
  guard (t1 == t2)

-- | Version of 'eqParam1' for types with 3 parameters.
eqParam3 ::
     forall a1 a2 b1 b2 c1 c2 t.
     ( Typeable a1
     , Typeable a2
     , Typeable b1
     , Typeable b2
     , Typeable c1
     , Typeable c2
     , Eq (t a1 b1 c1)
     )
  => t a1 b1 c1
  -> t a2 b2 c2
  -> Bool
eqParam3 t1 t2 = isJust @() $ do
  Refl <- eqT @a1 @a2
  Refl <- eqT @b1 @b2
  Refl <- eqT @c1 @c2
  guard (t1 == t2)
