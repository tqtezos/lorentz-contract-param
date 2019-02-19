-- | Module contains function to parse and typecheck Michelson contract.
-- Also contains some test functions (examples).

module Advanced.Parser
  ( parseAndTypeCheck
  ) where

import Data.Typeable ((:~:)(..), eqT)
import Data.Vinyl (Rec(..))
import Text.Megaparsec

import Advanced.Interpreter (ContractEnv(..), Operation, doInstr, run)
import Advanced.Type (CT(..), T(..))
import Advanced.TypeCheck (ContractInp, ContractOut, SomeContract(..), typeCheckC)
import Advanced.Value (CVal(..), Instr, Val(..))

import qualified Michelson.Types as M
import qualified Morley.Macro as Mo
import qualified Morley.Parser as Mo
import qualified Morley.Types as Mo

-- | Function parses for Michelson contract and type checks it.
--
-- First it parses contract to type and instruction representation from
-- @Michelson.Type@ module.
--
-- Then it converts parsed types of storage and parameter to singletons.
-- Parameter and storage singletons are used to convert instructions from
-- to advanced instruction representation with @Advanced.TypeCheck.typeCheck@
-- function.
parseAndTypeCheck :: Text -> Either Text (SomeContract Operation)
parseAndTypeCheck txt = do
    (M.Contract mParam mStorage pCode)
      <- either (Left . show) pure $ parse Mo.contract "" txt
    typeCheckC $
      M.Contract mParam mStorage (Mo.unOp <$> Mo.expandFlat pCode)

contractRes :: Rec (Val op cp) (ContractOut st) -> Val op cp st
contractRes (VPair (_, r) :& RNil) = r

_testParser :: Either Text [Integer]
_testParser = do
    parseAndTypeCheck (
      "parameter unit; storage (list (int :t)); "
      <> "code { CDR; PUSH int 0; SWAP; ITER { ADD; }; NIL operation;"
      <> "PUSH int 12; DIP { SWAP; }; ADD; PUSH (int :t) 100;"
      <> "NIL int; SWAP; CONS; SWAP; CONS; SWAP; PAIR; };"
      )
      >>=
        \(SomeContract (instr :: Instr Operation cp (ContractInp cp st)
                                                  (ContractOut st)) _ _) -> do
            Refl <- maybe (Left "Unexpected param type") pure $
                      eqT @cp @'T_unit
            Refl <- maybe (Left "Unexpected storage type") pure $
                      eqT @st @('T_list ('T_c 'T_int))
            let x = run instr (VPair (VUnit, (VList (
                                      (VC . CvInt) <$> [100, 10, 1]))) :& RNil)
            case doInstr x ContractEnv of
              Left _ -> Left "run error"
              Right r -> pure $ toList_ $ contractRes r
  where
    toList_ :: Val Operation 'T_unit ('T_list ('T_c 'T_int)) -> [Integer]
    toList_ (VList l) = map (\(VC (CvInt i)) -> i) l

_testParser2 :: Either Text (SomeContract Operation)
_testParser2 = do
  parseAndTypeCheck $
    "parameter (pair (int %x) (int :x));"
    <> "storage (list (pair int (int %y)));"
    <> "code { DUP; CAR; DIP { CDR; }; CONS; NIL operation; PAIR; };"

_testParser3 :: Either Text (SomeContract Operation)
_testParser3 = do
  parseAndTypeCheck $
    "parameter (pair (int %x) int);"
    <> "storage (list (pair (int %z) int));"
    <> "code { DUP; CAR; DIP { CDR; }; CONS; NIL operation; PAIR; };"

_testParser4 :: Either Text (SomeContract Operation)
_testParser4 = do
  parseAndTypeCheck $
    "parameter (pair (int %x) (int :x));"
    <> "storage (list (pair int (int %y :y)));"
    <> "code { DUP; CAR; DIP { CDR; }; CONS; NIL operation; PAIR; };"

