-- | Tests for the 'stringCaller.tz' contract.

module Test.Interpreter.StringCaller
  ( stringCallerSpec
  ) where

import Test.Hspec (Spec, it, parallel)

import Michelson.Typed
import Michelson.Untyped (OriginationOperation(..), mkContractAddress)
import qualified Michelson.Untyped as Untyped
import Morley.Aliases (UntypedContract)
import Morley.Runtime (InterpreterOp(..), TxData(..))
import Morley.Runtime.GState
import Morley.Test (specWithContract)
import Morley.Test.Integrational (expectStorageConstant)
import Tezos.Address (formatAddress)

import Test.Util.Interpreter

stringCallerSpec :: Spec
stringCallerSpec =
  parallel $
  specWithContract "contracts/stringCaller.tz" $ \stringCaller ->
  specWithContract "contracts/idString.tz" $ \idString ->
  specImpl stringCaller idString

specImpl ::
     (UntypedContract, Contract ('T_c 'T_string) ('T_c 'T_address))
  -> (UntypedContract, Contract ('T_c 'T_string) ('T_c 'T_string))
  -> Spec
specImpl (uStringCaller, _stringCaller) (uIdString, _idString) =
  it "stringCaller calls idString and updates its storage" $
    simplerIntegrationalTest operations (Right updatesValidator)
  where
    idStringOrigination :: OriginationOperation
    idStringOrigination =
      dummyOrigination (Untyped.ValueString "hello") uIdString
    originateIdString = OriginateOp idStringOrigination
    idStringAddress = mkContractAddress idStringOrigination

    stringCallerOrigination :: OriginationOperation
    stringCallerOrigination =
      dummyOrigination (Untyped.ValueString $ formatAddress idStringAddress)
      uStringCaller
    originateStringCaller = OriginateOp stringCallerOrigination
    stringCallerAddress = mkContractAddress stringCallerOrigination

    newValue = Untyped.ValueString "caller"
    txData :: TxData
    txData = TxData
      { tdSenderAddress = dummyContractAddress
      , tdParameter = newValue
      , tdAmount = minBound
      }
    transferToStringCaller = TransferOp stringCallerAddress txData

    operations =
      [ originateIdString
      , originateStringCaller
      , transferToStringCaller
      ]

    updatesValidator :: [GStateUpdate] -> Either Text ()
    updatesValidator updates =
      expectStorageConstant updates idStringAddress newValue
