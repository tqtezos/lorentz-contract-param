{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Module, containing parser from Michelson contract
-- to its representation as GADT.

module Advanced.Parser
  ( contractParser
  ) where

import Text.Megaparsec
import Data.Typeable (eqT, (:~:)(..))
import qualified Data.Set as S

import Advanced.Interpreter (Operation)
import Advanced.Type (Sing (..), T(..), fromMType, withSomeSingT)
import Advanced.TypeCheck (typeCheck, SomeInstr(..), SomeIT (..), IT (..))

import qualified Michelson.Types as M
import qualified Morley.Parser as Mo
import qualified Morley.Types as Mo
import qualified Morley.Macro as Mo

-- | Parser for Michelson contract (from text to advanced type representation).
--
-- First it parses contract to type and instruction representation from
-- @Michelson.Type@ module.
--
-- Then it converts parsed types of storage and parameter to singletons.
-- Parameter and storage singletons are used to convert instructions from
-- to advanced instruction representation with @Advanced.TypeCheck.typeCheck@
-- function.
contractParser :: Mo.Parser (SomeInstr Operation)
contractParser = do
    (M.Contract mParam mStorage pCode) <- Mo.contract
    code <- maybe (throwErr "no instructions in code") pure $
              nonEmpty $ Mo.expandFlat pCode
    withSomeSingT (fromMType mParam) $ \paramS ->
      withSomeSingT (fromMType mStorage) $ \(storageS :: Sing st) -> do
        let inp = ST_pair paramS storageS ::& INil
        either throwErr pure $ do
          r@(_ ::: (_, (_ :: IT out))) <- typeCheck @Operation (M.unOp <$> code) (SomeIT inp)
          Refl <- eqT' @out @'[ 'T_pair ('T_list 'T_operation) st ]
          pure r
  where
    throwErr = fancyFailure . S.singleton . ErrorCustom . Mo.OtherError

    eqT' :: forall (a :: [T]) (b :: [T]) .
            (Typeable a, Typeable b) => Either Text (a :~: b)
    eqT' = maybe (Left "Contract output type violates convention") pure eqT

testParser = do
  parse contractParser "myfile" $
    "parameter unit; storage (list int); "
    <> "code { CDR; PUSH int 0; SWAP; ITER { ADD; }; NIL operation; PUSH int 12; DIP { SWAP; }; ADD; PUSH int 100; NIL int; SWAP; CONS; SWAP; CONS; SWAP; PAIR; };"
