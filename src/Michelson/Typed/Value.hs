-- | Module, containing data types for Michelson value.

module Michelson.Typed.Value
  ( Val (..)
  , ContractInp
  , ContractOut
  , CreateAccount (..)
  , CreateContract (..)
  , CVal (..)
  , Operation (..)
  , SetDelegate (..)
  , TransferTokens (..)
  , ToVal
  , FromVal
  , toVal
  , fromVal
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Singletons (SingI)

import Michelson.Typed.CValue (CVal(..), FromCVal, ToCVal, fromCVal, toCVal)
import Michelson.Typed.T (T(..), ToT)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- | Data type, representing operation, list of which is returned
-- by Michelson contract (according to calling convention).
--
-- These operations are to be further executed against system state
-- after the contract execution.
data Operation instr where
  OpTransferTokens :: TransferTokens instr p -> Operation instr
  OpSetDelegate :: SetDelegate -> Operation instr
  OpCreateAccount :: CreateAccount -> Operation instr
  OpCreateContract
    :: (Show (instr (ContractInp cp st) (ContractOut st)), SingI cp, SingI st)
    => CreateContract instr t cp st
    -> Operation instr

deriving instance Show (Operation instr)

data TransferTokens instr p = TransferTokens
  { ttContractParameter :: !(Val instr p)
  , ttAmount :: !Mutez
  , ttContract :: !(Val instr ('TContract p))
  } deriving (Show)

data SetDelegate = SetDelegate
  { sdMbKeyHash :: !(Maybe KeyHash)
  } deriving (Show)

data CreateAccount = CreateAccount
  { caManager :: !KeyHash
  , caDelegate :: !(Maybe KeyHash)
  , caSpendable :: !Bool
  , caBalance :: !Mutez
  } deriving (Show)

data CreateContract instr t cp st
  = Show (instr (ContractInp cp st) (ContractOut st))
  => CreateContract
  { ccManager :: !KeyHash
  , ccDelegate :: !(Maybe KeyHash)
  , ccSpendable :: !Bool
  , ccDelegatable :: !Bool
  , ccBalance :: !Mutez
  , ccStorageVal :: !(Val instr t)
  , ccContractCode :: !(instr (ContractInp cp st) (ContractOut st))
  }

deriving instance Show (CreateContract instr t cp st)

type ContractInp param st = '[ 'TPair param st ]
type ContractOut st = '[ 'TPair ('TList 'TOperation) st ]

-- | Representation of Michelson value.
--
-- Type parameter @instr@ stands for Michelson instruction
-- type, i.e. data type to represent an instruction of language.
data Val instr t where
  VC :: CVal t -> Val instr ('Tc t)
  VKey :: PublicKey -> Val instr 'TKey
  VUnit :: Val instr 'TUnit
  VSignature :: Signature -> Val instr 'TSignature
  VOption :: Maybe (Val instr t) -> Val instr ('TOption t)
  VList :: [Val instr t] -> Val instr ('TList t)
  VSet :: Set (CVal t) -> Val instr ('TSet t)
  VOp :: Operation instr -> Val instr 'TOperation
  VContract :: Address -> Val instr ('TContract p)
  VPair :: (Val instr l, Val instr r) -> Val instr ('TPair l r)
  VOr :: Either (Val instr l) (Val instr r) -> Val instr ('TOr l r)
  VLam
    :: Show (instr '[inp] '[out])
    => instr (inp ': '[]) (out ': '[]) -> Val instr ('TLambda inp out)
  VMap :: Map (CVal k) (Val instr v) -> Val instr ('TMap k v)
  VBigMap :: Map (CVal k) (Val instr v) -> Val instr ('TBigMap k v)

deriving instance Show (Val instr t)

-- TODO: actually we should handle big maps with something close
-- to following:
--
--  VBigMap :: BigMap op ref k v -> Val cp ('TBigMap k v)
--
-- data ValueOp v
--     = New v
--     | Upd v
--     | Rem
--     | NotExisted
--
-- data BigMap op ref k v = BigMap
--  { bmRef :: ref k v, bmChanges :: Map (CVal k) (ValueOp (Val cp v)) }


-- | Converts a complex Haskell structure into @Val@ representation.
class ToVal a where
  toVal :: a -> Val instr (ToT a)

-- | Converts a @Val@ value into complex Haskell type.
class FromVal t where
  fromVal :: Val instr (ToT t) -> t

-- ToVal / FromVal instances

-- @gromak: we can write the following code instead of these
-- instances below, but I am not sure whether it's a good idea.
-- Note: if it breaks compilation for you, try to clean and
-- rebuild from scratch. It seems to compile fine.
-- instance {-# OVERLAPPABLE #-} ('Tc (ToCT t) ~ ToT t, FromCVal t) => FromVal t where
--   fromVal (VC cval) = fromCVal cval

instance FromVal Integer where
  fromVal (VC cval) = fromCVal cval

instance FromVal Natural where
  fromVal (VC cval) = fromCVal cval

instance FromVal Text where
  fromVal (VC cval) = fromCVal cval

instance FromVal Bool where
  fromVal (VC cval) = fromCVal cval

instance FromVal ByteString where
  fromVal (VC cval) = fromCVal cval

instance FromVal Mutez where
  fromVal (VC cval) = fromCVal cval

instance FromVal KeyHash where
  fromVal (VC cval) = fromCVal cval

instance FromVal Timestamp where
  fromVal (VC cval) = fromCVal cval

instance FromVal Address where
  fromVal (VC cval) = fromCVal cval

instance FromVal () where
  fromVal VUnit = ()

instance FromVal a => FromVal [a] where
  fromVal (VList lVals) = map fromVal lVals

instance FromVal a => FromVal (Maybe a) where
  fromVal (VOption Nothing) = Nothing
  fromVal (VOption (Just val)) = Just $ fromVal val

instance (FromVal a, FromVal b) => FromVal (Either a b) where
  fromVal (VOr (Left l)) = Left $ fromVal l
  fromVal (VOr (Right r)) = Right $ fromVal r

instance (FromVal a, FromVal b) => FromVal (a, b) where
  fromVal (VPair (a, b)) = (fromVal a, fromVal b)

instance (Ord k, FromCVal k) => FromVal (Set k) where
  fromVal (VSet s) = Set.map fromCVal s

instance (Ord k, FromCVal k, FromVal a) => FromVal (Map k a) where
  fromVal (VMap m) = Map.map fromVal $ Map.mapKeys fromCVal m

instance ToVal () where
  toVal _ = VUnit

instance ToVal Integer where
  toVal = VC . toCVal

instance ToVal Int where
  toVal = VC . toCVal

instance ToVal Word64 where
  toVal = VC . toCVal

instance ToVal Natural where
  toVal = VC . toCVal

instance ToVal Text where
  toVal = VC . toCVal

instance ToVal ByteString where
  toVal = VC . toCVal

instance ToVal Bool where
  toVal = VC . toCVal

instance ToVal Mutez where
  toVal = VC . toCVal

instance ToVal KeyHash where
  toVal = VC . toCVal

instance ToVal Timestamp where
  toVal = VC . toCVal

instance ToVal Address where
  toVal = VC . toCVal

instance ToVal a => ToVal (Maybe a) where
  toVal Nothing = VOption Nothing
  toVal (Just a) = VOption (Just $ toVal a)

instance (ToVal a, ToVal b) => ToVal (Either a b) where
  toVal (Left l) = VOr $ Left $ toVal l
  toVal (Right r) = VOr $ Right $ toVal r

instance (ToVal a, ToVal b) => ToVal (a, b) where
  toVal (l, r) = VPair (toVal l, toVal r)

instance ToVal x => ToVal [x] where
  toVal = VList . map toVal

instance ToCVal k => ToVal (Set k) where
  toVal = VSet . Set.map toCVal

-- Note: the instance produces Map not BigMap
instance (ToCVal k, ToVal a) => ToVal (Map k a) where
  toVal = VMap . Map.mapKeys toCVal . Map.map toVal
