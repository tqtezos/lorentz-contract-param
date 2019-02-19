{-# LANGUAGE DataKinds, GADTs #-}

-- | Module, providing @Notes t@ data type, which holds annotations for a
-- given type @t@.
--
-- Annotation type @Notes t@ is a tree, each leaf is either a star (@*@) or a
-- constructor holding some annotation data for a given type @t@.
-- Star corresponds to the case when given Michelson type contains no
-- annotations.
--
-- This module also provides type class 'Converge' along with some
-- utility functions which are used to combine two annotations trees
-- `a` and `b` into a new one `c` in such a way that `c` can be obtained from
-- both `a` and `b` by replacing some @*@ leafs with type or/and field
-- annotations.

module Advanced.Type.Annotation
  (
    Notes (..)
  , Notes' (..)
  , Converge (..)
  , convergeAnns
  , notesCase
  , isStar
  , mkNotes
  , mkNotes0
  , orAnn
  ) where

import Data.Default (Default(..))

import Advanced.Type.T (T(..))
import Michelson.Types (Annotation, FieldAnn, TypeAnn, unifyAnn)


-- Data type, holding annotation data for a given Michelson type @t@
-- or @*@ in case no data is provided for the tree.
data Notes t = N (Notes' t) | NStar

-- | Helper function for work with 'Notes' data type.
--
-- @
--  notesCase f g notes
-- @
--
-- is equivalent to
--
-- @
--  case notes of
--    NStar -> f
--    N v -> g v
-- @
--
notesCase :: r -> (Notes' t -> r) -> Notes t -> r
notesCase a _ NStar = a
notesCase _ f (N b) = f b

-- | Data type, holding annotation data for a given Michelson type @t@.
--
-- Each constructor corresponds to exactly one constructor of 'T'
-- and holds all type and field annotations that can be attributed to a
-- Michelson type corrspoding to @t@.
data Notes' t where
  NT_c         :: TypeAnn -> Notes' ('T_c ct)
  NT_key       :: TypeAnn -> Notes' 'T_key
  NT_unit      :: TypeAnn -> Notes' 'T_unit
  NT_signature :: TypeAnn -> Notes' 'T_signature
  NT_option    :: Converge t
               => TypeAnn -> FieldAnn -> Notes t -> Notes' ('T_option t)
  NT_list      :: Converge t => TypeAnn -> Notes t -> Notes' ('T_list t)
  NT_set       :: TypeAnn -> TypeAnn -> Notes' ('T_set ct)
  NT_operation :: TypeAnn -> Notes' 'T_operation
  NT_contract  :: Converge t => TypeAnn -> Notes t -> Notes' ('T_contract t)
  NT_pair      :: (Converge p, Converge q)
               => TypeAnn -> FieldAnn -> FieldAnn
               -> Notes p -> Notes q -> Notes' ('T_pair p q)
  NT_or        :: (Converge p, Converge q)
               => TypeAnn -> FieldAnn -> FieldAnn
               -> Notes p -> Notes q -> Notes' ('T_or p q)
  NT_lambda    :: (Converge p, Converge q)
               => TypeAnn -> Notes p -> Notes q -> Notes' ('T_lambda p q)
  NT_map       :: Converge v
               => TypeAnn -> TypeAnn -> Notes v -> Notes' ('T_map k v)
  NT_big_map   :: Converge v
               => TypeAnn -> TypeAnn -> Notes v -> Notes' ('T_big_map k v)

-- | Check whether given annotations object is @*@.
isStar :: Notes t -> Bool
isStar NStar = True
isStar _ = False

isDef :: (Eq t, Default t) => t -> Bool
isDef = (== def)

-- | Checks whether given notes @n@ can be immediately converted to star
-- and returns either @NStar@ or @N n@.
mkNotes :: Notes' t -> Notes t
mkNotes (NT_option tn fn ns) | isStar ns && isDef tn && isDef fn   = NStar
mkNotes (NT_list tn ns)      | isStar ns && isDef tn               = NStar
mkNotes (NT_set tn en)       | isDef tn && isDef en                = NStar
mkNotes (NT_contract tn ns)  | isStar ns && isDef tn               = NStar
mkNotes (NT_pair tn fn1 fn2 ns1 ns2)
  | isStar ns1 && isStar ns2 && isDef tn && isDef fn1 && isDef fn2 = NStar
mkNotes (NT_or tn fn1 fn2 ns1 ns2)
  | isStar ns1 && isStar ns2 && isDef tn && isDef fn1 && isDef fn2 = NStar
mkNotes (NT_lambda tn ns1 ns2)
  | isStar ns1 && isStar ns2 && isDef tn                           = NStar
mkNotes (NT_map tn kn vns)
  | isStar vns && isDef tn && isDef kn                             = NStar
mkNotes n = N n

orAnn :: Annotation t -> Annotation t -> Annotation t
orAnn a b = bool a b (a == def)

mkNotes0
  :: (TypeAnn -> Notes' t)
  -> TypeAnn -> Notes t
mkNotes0 f n = bool (N $ f n) NStar (isDef n)

-- | Class, which defines methods for combining values of 'Notes' and 'Notes''
-- data types.
class Converge t where
  -- | Combines two annotations trees `a` and `b` into a new one `c`
  -- in such a way that `c` can be obtained from both `a` and `b` by replacing
  -- some @*@ leafs with type or/and field annotations.
  converge' :: Notes' t -> Notes' t -> Either Text (Notes' t)

  -- | Same as 'converge'' but works with 'Notes' data type.
  converge :: Notes t -> Notes t -> Either Text (Notes t)
  converge NStar a = pure a
  converge a NStar = pure a
  converge (N a) (N b) = N <$> converge' a b

-- | Converge two type or field notes (which may be wildcards).
convergeAnns
  :: Show (Annotation tag)
  => Annotation tag -> Annotation tag -> Either Text (Annotation tag)
convergeAnns a b = maybe (Left $ "Annotations do not converge: "
                            <> show a <> " /= " <> show b)
                          pure $ unifyAnn a b

instance Converge ('T_c ct) where
  converge' (NT_c a) (NT_c b) = NT_c <$> convergeAnns a b
instance Converge 'T_key where
  converge' (NT_key a) (NT_key b) = NT_key <$> convergeAnns a b
instance Converge 'T_unit where
  converge' (NT_unit a) (NT_unit b) = NT_unit <$> convergeAnns a b
instance Converge 'T_signature where
  converge' (NT_signature a) (NT_signature b) =
      NT_signature <$> convergeAnns a b
instance Converge t => Converge ('T_option t) where
  converge' (NT_option a f n) (NT_option b g m) =
    NT_option <$> convergeAnns a b <*> convergeAnns f g <*> converge n m
instance Converge t => Converge ('T_list t) where
  converge' (NT_list a n) (NT_list b m) =
    NT_list <$> convergeAnns a b <*> converge n m
instance Converge ('T_set t) where
  converge' (NT_set a n) (NT_set b m) =
    NT_set <$> convergeAnns a b <*> convergeAnns n m
instance Converge 'T_operation where
  converge' (NT_operation a) (NT_operation b) =
    NT_operation <$> convergeAnns a b
instance Converge t => Converge ('T_contract t) where
  converge' (NT_contract a n) (NT_contract b m) =
    NT_contract <$> convergeAnns a b <*> converge n m
instance (Converge p, Converge q) => Converge ('T_pair p q) where
  converge' (NT_pair a pF qF pN qN) (NT_pair b pG qG pM qM) =
    NT_pair <$> convergeAnns a b <*> convergeAnns pF pG
            <*> convergeAnns qF qG <*> converge pN pM <*> converge qN qM
instance (Converge p, Converge q) => Converge ('T_or p q) where
  converge' (NT_or a pF qF pN qN) (NT_or b pG qG pM qM) =
    NT_or <$> convergeAnns a b <*> convergeAnns pF pG <*> convergeAnns qF qG
            <*> converge pN pM <*> converge qN qM
instance (Converge p, Converge q) => Converge ('T_lambda p q) where
  converge' (NT_lambda a pN qN) (NT_lambda b pM qM) =
    NT_lambda <$> convergeAnns a b <*> converge pN pM <*> converge qN qM
instance Converge v => Converge ('T_map k v) where
  converge' (NT_map a kN vN) (NT_map b kM vM) =
    NT_map <$> convergeAnns a b <*> convergeAnns kN kM <*> converge vN vM
instance Converge v => Converge ('T_big_map k v) where
  converge' (NT_big_map a kN vN) (NT_big_map b kM vM) =
    NT_big_map <$> convergeAnns a b <*> convergeAnns kN kM <*> converge vN vM
