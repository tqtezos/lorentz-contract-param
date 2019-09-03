{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.SomeContractParam where

import Control.Monad
import Data.String
import GHC.Generics
import Michelson.Typed.T
import Prelude (Typeable, ReaderT(..), Maybe(..), Either(..), Bool(..), error, ($), either, flip, (.))
import Text.Show
import qualified Prelude as P

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Constraint
import Data.Default
import Data.Semigroup
import Data.Singletons
import Data.Type.Equality

import Michelson.TypeCheck
import Michelson.Typed

deriving instance Generic T

instance ToJSON T where
instance FromJSON T where

-- | `runTypeCheckTest` lifted to `TypeCheckInstr`
runTypeCheckInstr :: TypeCheckInstr a -> Either TCError a
runTypeCheckInstr = runTypeCheckTest . flip runReaderT def

-- | A contract parameter with some type
data SomeContractParam where
  SomeContractParam
    :: (SingI t, Typeable t)
    => Value t
    -> (Sing t, Notes t)
    -> (Dict (HasNoOp t), Dict (HasNoBigMap t))
    -> SomeContractParam

-- | `eqType` specialized to allow passing @a, b@ using `Sing`
eqTypeSing ::
     forall (a :: T) (b :: T). P.Each '[ Typeable, SingI] '[ a, b]
  => Sing a
  -> Sing b
  -> Either TCTypeError (a :~: b)
eqTypeSing _ _ = eqType

instance P.Eq SomeContractParam where
  paramX == paramY =
    case paramX of
      SomeContractParam xs (singX, _) (Dict, Dict) ->
        case paramY of
          SomeContractParam ys (singY, _) (Dict, Dict) ->
            case eqTypeSing singX singY of
              Left _ -> False
              Right Refl -> xs P.== ys

instance Show SomeContractParam where
  show (SomeContractParam xs (_, _) (Dict, Dict)) = show xs

-- | Convert an `IsoValue` value to `SomeContractParam`
toSomeContractParam ::
     ( IsoValue t
     , SingI (ToT t)
     , Typeable (ToT t)
     , HasNoOp (ToT t)
     , HasNoBigMap (ToT t)
     )
  => t
  -> SomeContractParam
toSomeContractParam xs =
  let singT = sing
   in case extractNotes (toUType $ fromSing singT) singT of
        Left err -> error $ P.show err
        Right notesT ->
          SomeContractParam (toVal xs) (singT, notesT) (Dict, Dict)

-- | Consume `SomeContractParam`
fromSomeContractParam ::
     SomeContractParam
  -> (forall t. (SingI t, Typeable t, HasNoOp t, HasNoBigMap t) =>
                  Value t -> r)
  -> r
fromSomeContractParam (SomeContractParam xs (_, _) (Dict, Dict)) f = f xs


instance ToJSON SomeContractParam where
  toJSON (SomeContractParam xs (sing', _) (Dict, _)) =
    toJSON (fromSing sing', untypeValue xs)

instance FromJSON SomeContractParam where
  parseJSON x = do
    (t, uValue) <- parseJSON x
    withSomeSingT t $ \singT -> do
      notes' <- either (fail . show) return $ extractNotes (toUType t) singT
      ((::::) xs (sing', notesT)) <-
        either (fail . show) return . runTypeCheckInstr $
        typeCheckValue uValue (singT, notes')
      case opAbsense sing' of
        Nothing -> error $ "SomeContractParam contains operation: " <> P.show xs
        Just dictHasNoOp ->
          case bigMapAbsense sing' of
            Nothing -> error $ "SomeContractParam contains bigmap" <> P.show xs
            Just dictHasNoBigMap ->
              return $
              SomeContractParam
                xs
                (sing', notesT)
                (dictHasNoOp, dictHasNoBigMap)

