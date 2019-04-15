-- | Module, containing spec to test contract_op.tz contract.
module Test.Interpreter.ContractOp
  ( contractOpSpec
  ) where

import qualified Data.Map as M
import Test.Hspec (Spec, describe, it, parallel)
import Test.QuickCheck (Property, (===))

import Michelson.Interpret (ContractEnv(..), ContractReturn)
import Michelson.Test (contractProp, dummyContractEnv, failedProp, specWithTypedContract)
import Michelson.Typed (Contract, ToT, fromVal)
import Michelson.Untyped (CT(..), T(..), Type(..), noAnn)
import Tezos.Address (Address, unsafeParseAddress)

-- | Spec to test compare.tz contract.
contractOpSpec :: Spec
contractOpSpec = parallel $ describe "CONTRACT instruction tests" $ do
  specWithTypedContract "contracts/contract_op.tz" $ \contract -> do
    it "contract not found" $
      contractProp' False [] contract

    it "contract found, expected parameter is int :q, actual is int :q" $
      contractProp' True [(addr, intQ)] contract
    it "contract found, expected parameter int :q, actual int" $
      contractProp' True [(addr, int)] contract

    it "contract found, but expected parameter is int :p, actual is int :q" $
      contractProp' False [(addr, intP)] contract
    it "contract found, but expected parameter is int :p, actual is string" $
      contractProp' False [(addr, string)] contract
  where
    intQ = Type (Tc CInt) "q"
    int = Type (Tc CInt) noAnn
    intP = Type (Tc CInt) "p"
    string = Type (Tc CString) noAnn

    addr = unsafeParseAddress "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9"

    validate
      :: Bool
      -> ContractReturn s (ToT Bool)
      -> Property
    validate ex (Right ([], fromVal -> l), _) = l === ex
    validate _ (Left _, _) = failedProp "Unexpected fail in interepreter"
    validate _ _ = failedProp "Unexpected result of script execution"

    contractProp' :: Bool -> [(Address, Type)] -> Contract (ToT Address) (ToT Bool) -> Property
    contractProp' res ctrs contract =
      contractProp
        contract
        (validate res)
        dummyContractEnv {ceContracts = M.fromList ctrs}
        addr
        False
