module Main
  ( main
  ) where

import Data.Char
import Data.Functor.Classes
import Data.List (isSuffixOf, unlines, unwords)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (readEither, unlines, unwords)
import System.Environment
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

import Data.Singletons (SingI(..))
import Fmt (Builder, blockListF, fmt, nameF, (+|), (|+))
import Lorentz.Contracts.Auction
import Lorentz.Contracts.ManagedLedger.Athens (managedLedgerAthensContract)
import Lorentz.Contracts.ManagedLedger.Babylon (managedLedgerContract)
import Lorentz.Contracts.ManagedLedger.Proxy (managedLedgerProxyContract)
import Lorentz.Contracts.UnsafeLedger
import Lorentz.Contracts.Walker
import Lorentz.Macro
import Michelson.Printer.Util
import Michelson.Typed.Convert
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Named
import Tezos.Address
import Tezos.Crypto
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Lorentz.Base as L
import qualified Lorentz.Contracts.Auction as Auction
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon
import qualified Lorentz.Contracts.ManagedLedger.Proxy as Proxy
import qualified Lorentz.Contracts.UnsafeLedger as UnsafeLedger
import qualified Lorentz.Contracts.Walker as Walker
import qualified Lorentz.Doc as L


deriving instance Read KeyHash
deriving instance Read Babylon.Parameter
deriving instance Read UnsafeLedger.Parameter
deriving instance Read Walker.Parameter

deriving instance Read Proxy.Parameter
deriving instance Read Proxy.Parameter0
deriving instance Read Proxy.Parameter1
deriving instance Read Proxy.Parameter2


inParensP :: ReadP a -> ReadP a
inParensP = P.char '(' `P.between` P.char ')'

maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = liftM2 (<|>) inParensP id

readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . (P.between `join` P.char '"') $ do
        addressStr <- P.munch1 isAlphaNum
        case parseAddress $ T.pack addressStr of
          Left err -> fail $ show err
          Right address' -> return address'

instance Read Athens.Parameter where
  readPrec =
    choice
      [ readUnaryWith (parens readPrec) "Transfer" Athens.Transfer
      , readUnaryWith (parens readPrec) "TransferViaProxy" Athens.TransferViaProxy
      , readUnaryWith (parens readPrec) "Approve" Athens.Approve
      , readUnaryWith (parens readPrec) "ApproveViaProxy" Athens.ApproveViaProxy
      , readUnaryWith (parens readPrec) "GetAllowance" Athens.GetAllowance
      , readUnaryWith (parens readPrec) "GetBalance" Athens.GetBalance
      , readUnaryWith (parens readPrec) "GetTotalSupply" Athens.GetTotalSupply
      , readUnaryWith (parens readPrec) "SetPause" Athens.SetPause
      , readUnaryWith (parens readPrec) "SetAdministrator" Athens.SetAdministrator
      , readUnaryWith (parens readPrec) "GetAdministrator" Athens.GetAdministrator
      , readUnaryWith (parens readPrec) "Mint" Athens.Mint
      , readUnaryWith (parens readPrec) "Burn" Athens.Burn
      , readUnaryWith (parens readPrec) "SetProxy" Athens.SetProxy
      ]

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP

instance Read (ContractAddr cp) where
  readPrec =
    readP_to_Prec $ \prec -> do
      P.string "ContractAddr"
      P.skipSpaces
      ContractAddr <$> readAddressP

instance Read a => Read (View a r) where
  readPrec =
    readP_to_Prec $ \prec -> do
      P.skipSpaces
      P.string "View"
      P.skipSpaces
      arg <- readPrec_to_P (parens readPrec) prec
      P.skipSpaces
      View arg . ContractAddr <$> readAddressP

instance (Read a, KnownSymbol name) => Read (NamedF Identity a name) where
  readPrec = readPrec' Proxy
    where
      readPrec' ::
           (Read a', KnownSymbol name')
        => proxy name'
        -> ReadPrec (NamedF Identity a' name')
      readPrec' nameProxy =
        readP_to_Prec $ \prec -> do
          P.skipSpaces
          (P.between `join` P.char '"') . P.string $ symbolVal nameProxy
          P.skipSpaces
          P.string ".!"
          P.skipSpaces
          ArgF . Identity <$> readPrec_to_P readPrec prec


-- | Read and render (or fail with a `String` error message)
-- the parameter type associated with a `L.Contract`
contractReadAndRenderParam ::
     (Read p, IsoValue p, SingI (ToT p), HasNoOp (ToT p))
  => L.Contract p s
  -> String
  -> Either String String
contractReadAndRenderParam contract' =
  fmap (show . renderDoc . untypeValue . toVal) . readAsContractType contract'
  where
    readAsContractType ::
         Read p' => L.Contract p' s' -> String -> Either String p'
    readAsContractType _ = readEither

-- | `Map` from contract names to readers generated with
-- `contractReadAndRenderParam`
contractReadableIsoValues :: Map String (String -> Either String String)
contractReadableIsoValues =
  Map.fromList
    [ ("ManagedLedger", contractReadAndRenderParam managedLedgerContract)
    , ("ManagedLedgerAthens", contractReadAndRenderParam managedLedgerAthensContract)
    , ("ManagedLedgerProxy", contractReadAndRenderParam managedLedgerProxyContract)
    , ("UnsafeLedger", contractReadAndRenderParam unsafeLedgerContract)
    , ("Walker", contractReadAndRenderParam walkerContract)
    , ("Auction", contractReadAndRenderParam auctionContract)
    ]

-- | A `Builder` containing available contracts
availableContracts :: Builder
availableContracts =
  nameF "Available contracts" (blockListF $ keys contractReadableIsoValues)

contractNameReadRenderParam :: String -> String -> Either String String
contractNameReadRenderParam contractName parameterStr =
  case Map.lookup contractName contractReadableIsoValues of
    Nothing -> do
      Left $ unwords ["Error: Contract name:", contractName, "is not valid"]
    Just parseContractParams -> do
      case parseContractParams parameterStr of
        Left errorStr -> do
          Left . unlines . fmap unwords $
            [ ["Error: Contract parameter invalid"] -- :", parameterStr]
            , ["Failed with:", errorStr]
            ]
        Right parsedContractParams -> return parsedContractParams

main :: IO ()
main = do
  case forM_ readAndRenderTests $ mapM (uncurry contractNameReadRenderParam) . join (,) of
    Left err -> error $ T.pack err
    Right () -> return ()

  args <- getArgs
  let printHelp = do
        fmt availableContracts
        putStrLn $ unlines [ "Usage: lorentz-contract-param [contract name] [Haskell parameter]" , "" , "E.g." , "$ lorentz-contract-param ManagedLedgerAthens SetPause False" ]
  case args of
    [] -> printHelp
    (contractName:paramWords) -> do
      if "help" `isSuffixOf` contractName
         then do
           printHelp
           putStrLn ("Example parameters:" :: String)
           forM_ readAndRenderTests $ \(name, param) -> do
             putStrLn name
             putStrLn param
             putStrLn ("" :: String)
         else do
           let parameterStr = unwords paramWords
           case contractNameReadRenderParam contractName parameterStr of
             Left err -> printHelp
             Right result -> putStrLn result

readAndRenderTests :: [(String, String)]
readAndRenderTests = fmap ((,) "ManagedLedgerAthens") managedLedgerAthensTests
  where
    managedLedgerAthensTests :: [String]
    managedLedgerAthensTests =
      [ "Transfer (\"from\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"to\" .! \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\", \"value\" .! 0)"
      , "Approve          (\"spender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 3)"
      , "Mint             (\"to\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 4)"
      , "Burn             (\"from\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 4)"
      , "SetPause         False"
      , "ApproveViaProxy  (\"sender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", (\"spender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"value\" .! 3))"
      , "SetAdministrator \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\""
      , "SetProxy \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\""
      , "SetProxy \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\""
      , "GetTotalSupply (View () \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
      , "GetAdministrator (View () \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
      , "GetAllowance (View (\"owner\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\", \"spender\" .! \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\") \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
      , "GetBalance (View \"tz1MhGthgRDEK4J5VVzt7r9sBS8FE462DAtr\" \"KT1T32YJo9La2czLFpeXPW9J5fVSCt1MHUtx\")"
      ]

