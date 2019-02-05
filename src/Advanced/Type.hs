{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Advanced.Type where

data T =
    T_c CT
  | T_key
  | T_unit
  | T_signature
  | T_option T
  | T_list T
  | T_set CT
  | T_operation
  | T_contract T
  | T_pair T T
  | T_or T T
  | T_lambda T T
  | T_map CT T
  | T_big_map CT T

data CT =
    T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address
  deriving (Eq, Show)

data SomeProxy k where
  SomeProxy :: Typeable t => Proxy (t :: k) -> SomeProxy k

withSomeProxy
  :: SomeProxy k
  -> (forall t . Typeable t => Proxy (t :: k) -> b)
  -> b
withSomeProxy (SomeProxy p) f = f p

ctProxy :: CT -> SomeProxy CT
ctProxy T_int = SomeProxy @CT (Proxy @'T_int)
ctProxy T_nat = SomeProxy @CT (Proxy @'T_nat)
ctProxy T_string = SomeProxy @CT (Proxy @'T_string)
ctProxy T_bytes = SomeProxy @CT (Proxy @'T_bytes)
ctProxy T_mutez = SomeProxy @CT (Proxy @'T_mutez)
ctProxy T_bool = SomeProxy @CT (Proxy @'T_bool)
ctProxy T_key_hash = SomeProxy @CT (Proxy @'T_key_hash)
ctProxy T_timestamp = SomeProxy @CT (Proxy @'T_timestamp)
ctProxy T_address = SomeProxy @CT (Proxy @'T_address)

tProxy :: T -> SomeProxy T
tProxy (T_c ct) = withSomeProxy (ctProxy ct) $ \(_ :: Proxy ct) -> SomeProxy @T (Proxy @('T_c ct))
tProxy T_key = SomeProxy @T (Proxy @'T_key)
tProxy T_unit = SomeProxy @T (Proxy @'T_unit)
tProxy T_signature = SomeProxy @T (Proxy @'T_signature)
tProxy (T_option t) = withSomeProxy (tProxy t) $ \(_ :: Proxy t) -> SomeProxy @T (Proxy @('T_option t))
tProxy (T_list t) = withSomeProxy (tProxy t) $ \(_ :: Proxy t) -> SomeProxy @T (Proxy @('T_list t))
tProxy (T_set ct) = withSomeProxy (ctProxy ct) $ \(_ :: Proxy ct) -> SomeProxy @T (Proxy @('T_set ct))
tProxy T_operation = SomeProxy @T (Proxy @'T_operation)
tProxy (T_contract t) = withSomeProxy (tProxy t) $ \(_ :: Proxy t) -> SomeProxy @T (Proxy @('T_contract t))
tProxy (T_pair l r) =
  withSomeProxy (tProxy l) $ \(_ :: Proxy l) ->
  withSomeProxy (tProxy r) $ \(_ :: Proxy r) ->
    SomeProxy @T (Proxy @('T_pair l r))
tProxy (T_or l r) =
  withSomeProxy (tProxy l) $ \(_ :: Proxy l) ->
  withSomeProxy (tProxy r) $ \(_ :: Proxy r) ->
    SomeProxy @T (Proxy @('T_or l r))
tProxy (T_lambda l r) =
  withSomeProxy (tProxy l) $ \(_ :: Proxy l) ->
  withSomeProxy (tProxy r) $ \(_ :: Proxy r) ->
    SomeProxy @T (Proxy @('T_lambda l r))
tProxy (T_map k v) =
  withSomeProxy (ctProxy k) $ \(_ :: Proxy k) ->
  withSomeProxy (tProxy v) $ \(_ :: Proxy v) ->
    SomeProxy @T (Proxy @('T_map k v))
tProxy (T_big_map k v) =
  withSomeProxy (ctProxy k) $ \(_ :: Proxy k) ->
  withSomeProxy (tProxy v) $ \(_ :: Proxy v) ->
    SomeProxy @T (Proxy @('T_big_map k v))
