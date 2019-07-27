-- | Module, containing spec to test contract_op.tz contract.
module Test.Interpreter.ContractOp
  ( test_contract_op
  ) where

import qualified Data.Map as M
import Test.QuickCheck (Property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

import Michelson.Interpret (ContractEnv(..), ContractReturn)
import Michelson.Test (contractProp, dummyContractEnv, failedProp, testTreesWithTypedContract)
import Michelson.Typed (Contract, ToT, fromVal)
import Michelson.Untyped (CT(..), T(..), Type(..), noAnn)
import Tezos.Address (Address, unsafeParseAddress)

-- | Spec to test compare.tz contract.
test_contract_op :: IO [TestTree]
test_contract_op =
  testTreesWithTypedContract "contracts/contract_op.tz" $ \contract -> pure
  [ testProperty "contract not found" $
      contractProp' False [] contract
  , testProperty "contract found, expected parameter is int :q, actual is int :q" $
      contractProp' True [(addr, intQ)] contract
  , testProperty "contract found, expected parameter int :q, actual int" $
      contractProp' True [(addr, int)] contract
  , testProperty "contract found, but expected parameter is int :p, actual is int :q" $
      contractProp' False [(addr, intP)] contract
  , testProperty "contract found, but expected parameter is int :p, actual is string" $
      contractProp' False [(addr, string)] contract
  ]
  where
    intQ = Type (Tc CInt) "q"
    int = Type (Tc CInt) noAnn
    intP = Type (Tc CInt) "p"
    string = Type (Tc CString) noAnn

    addr = unsafeParseAddress "KT1WsLzQ61xtMNJHfwgCHh2RnALGgFAzeSx9"

    validate
      :: Bool
      -> ContractReturn (ToT Bool)
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
