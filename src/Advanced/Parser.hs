-- | Module, containing parser from Michelson contract
-- to its representation as GADT.

module Advanced.Parser
  ( contractParser
  ) where

import Data.Default (Default(..))
import qualified Data.Set as S
import Data.Typeable ((:~:)(..), eqT)
import Data.Vinyl (Rec(..))
import Text.Megaparsec

import Advanced.CValue (CVal(..))
import Advanced.Interpreter (Operation, run)
import Advanced.Type
  (CT(..), Notes(..), Notes'(..), Sing(..), T(..), converge, extractNotes, fromMType, isStar,
  withSomeSingT)
import Advanced.TypeCheck (IT(..), SomeIT(..), SomeInstr(..), typeCheck)
import Advanced.Value (Instr, Val(..))

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
      withSomeSingT (fromMType mStorage) $ \(storageS :: Sing st) ->
        either throwErr pure $ do
          storageNote <- extractNotes mStorage storageS
          paramNote <- extractNotes mParam paramS
          let inpNote =
                bool (N $ NT_pair def def def paramNote storageNote)
                     NStar (isStar storageNote && isStar paramNote)
          let inp = (ST_pair paramS storageS, inpNote) ::& INil
          r@(_ ::: (_, (out :: IT out)))
              <- typeCheck @Operation (M.unOp <$> code) (SomeIT inp)
          Refl <- eqT' @out @(ContractOut st)
          let outN = outNotes out
          _ <- converge outN
                        (N $ NT_pair def def def NStar storageNote)
          pure r
  where
    outNotes :: IT '[o] -> Notes o
    outNotes ((_, n) ::& INil) = n

    throwErr = fancyFailure . S.singleton . ErrorCustom . Mo.OtherError

    eqT' :: forall (a :: [T]) (b :: [T]) .
            (Typeable a, Typeable b) => Either Text (a :~: b)
    eqT' = maybe (Left "Contract output type violates convention") pure eqT

type ContractInp param st = '[ 'T_pair param st ]
type ContractOut st = '[ 'T_pair ('T_list 'T_operation) st ]

testParserDo :: Text -> Either Text (SomeInstr Operation)
testParserDo = either (Left . show) pure . parse contractParser ""

contractRes :: Rec (Val op) (ContractOut st) -> Val op st
contractRes (VPair (_, r) :& RNil) = r

_testParser :: Either Text [Integer]
_testParser = do
    testParserDo (
      "parameter unit; storage (list (int :t)); "
      <> "code { CDR; PUSH int 0; SWAP; ITER { ADD; }; NIL operation;"
      <> "PUSH int 12; DIP { SWAP; }; ADD; PUSH (int :t) 100;"
      <> "NIL int; SWAP; CONS; SWAP; CONS; SWAP; PAIR; };"
      )
      >>= \((instr :: Instr Operation inp out) ::: _) -> do
            Refl <- maybe (Left "Unexpected input type") pure $
              eqT @inp @(ContractInp 'T_unit ('T_list ('T_c 'T_int)))
            Refl <- maybe (Left "Unexpected output type") pure $
              eqT @out @(ContractOut ('T_list ('T_c 'T_int)))
            pure $ toList_ $ contractRes $
              run instr (VPair (VUnit, (VList ((VC . CvInt) <$> [100, 10, 1]))) :& RNil)
  where
    toList_ :: Val Operation ('T_list ('T_c 'T_int)) -> [Integer]
    toList_ (VList l) = map (\(VC (CvInt i)) -> i) l

_testParser2 :: Either Text (SomeInstr Operation)
_testParser2 = do
  testParserDo $
    "parameter (pair (int %x) (int :x));"
    <> "storage (list (pair int (int %y)));"
    <> "code { DUP; CAR; DIP { CDR; }; CONS; NIL operation; PAIR; };"

_testParser3 :: Either Text (SomeInstr Operation)
_testParser3 = do
  testParserDo $
    "parameter (pair (int %x) int);"
    <> "storage (list (pair (int %z) int));"
    <> "code { DUP; CAR; DIP { CDR; }; CONS; NIL operation; PAIR; };"

_testParser4 :: Either Text (SomeInstr Operation)
_testParser4 = do
  testParserDo $
    "parameter (pair (int %x) (int :x));"
    <> "storage (list (pair int (int %y :y)));"
    <> "code { DUP; CAR; DIP { CDR; }; CONS; NIL operation; PAIR; };"

