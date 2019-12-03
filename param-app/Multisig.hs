{-# LANGUAGE BangPatterns #-}

module Multisig where

import Control.Applicative
import Data.Char
import Data.List
import Data.Typeable
import Data.String
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (readEither, unlines, unwords, show, null)
import Text.Show
import qualified Prelude as P

import Data.Aeson hiding (Value)
-- import Data.Aeson.TH
-- import Language.Haskell.TH.Ppr
-- import Language.Haskell.TH.Lib

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Constraint
import Data.Singletons (SingI(..))
import Data.Version (showVersion)
import Named
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_lorentz_contract_param (version)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Types as Opt
import Control.Monad.Except

import Lorentz hiding (contractName)
import Lorentz.Contracts.Auction
import Lorentz.Contracts.ManagedLedger.Babylon (managedLedgerContract)
import Lorentz.Contracts.ManagedLedger.Proxy (managedLedgerProxyContract)
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.UnsafeLedger
import Lorentz.Contracts.Util ()
import Lorentz.Contracts.VarStorage
import Lorentz.Contracts.Walker
import Michelson.Interpret.Pack
import Michelson.Macro
import Michelson.Parser
import Michelson.Printer.Util
import Michelson.Runtime
-- import Michelson.TypeCheck
import Michelson.Typed
import Tezos.Crypto (SecretKey)
-- import Util.IO
-- import Util.Named
-- import qualified Lorentz.Base as L
import qualified Lorentz.Contracts.GenericMultisig as G
-- import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G
-- import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
-- import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon
import qualified Tezos.Crypto as Crypto


import System.Directory

-- | A file with everything needed to sign some multisig contract parameters
data MultisigSignersFile key where
  MultisigSignersFile :: forall key. G.IsKey key =>
    { keyProxy :: Proxy key
    , contractName :: String
    , contractAddress :: Address
    , counter :: Natural
    , contractParameter :: Either (G.ChangeKeyParams key) SomeContractParam
    , publicKeys :: [G.Public key]
    , signatures :: Map (G.Public key) (Maybe (Either (G.PartialSig key) (G.Sig key)))
    } -> MultisigSignersFile key

instance ToJSON (MultisigSignersFile key) where
  toJSON MultisigSignersFile{..} =
    object $
      [ "contractName" .= contractName
      , "contractAddress" .= contractAddress
      , "counter" .= counter
      , "contractParameter" .= contractParameter
      , "publicKeys" .= publicKeys
      , "signatures" .= signatures
      ]

  toEncoding MultisigSignersFile{..} =
    pairs $
        "contractName" .= contractName <>
        "contractAddress" .= contractAddress <>
        "counter" .= counter <>
        "contractParameter" .= contractParameter <>
        "publicKeys" .= publicKeys <>
        "signatures" .= signatures

instance G.IsKey key => FromJSON (MultisigSignersFile key) where
  parseJSON =
    withObject "MultisigSignersFile" $
      \v -> MultisigSignersFile (Proxy @key)
        <$> v .: "contractName"
        <*> v .: "contractAddress"
        <*> v .: "counter"
        <*> v .: "contractParameter"
        <*> v .: "publicKeys"
        <*> v .: "signatures"

instance G.IsKey key => Semigroup (MultisigSignersFile key) where
  fileX <> fileY
    | contractNameX /= contractNameY =
      error $
      "MultisigSignersFile: contract names do not match: " <>
      P.show (contractNameX, contractNameY)
    | contractAddressX /= contractAddressY =
      error $
      "MultisigSignersFile: contract addresses do not match: " <>
      P.show (contractAddressX, contractAddressY)
    | counterX /= counterY =
      error $
      "MultisigSignersFile: counters do not match: " <>
      P.show (counterX, counterY)
    | contractParameterX /= contractParameterY =
      error $
      "MultisigSignersFile: contact parameters do not match: " <>
      P.show (contractParameterX, contractParameterY)
    | Map.keys signaturesX /= Map.keys signaturesY =
      error $
      "MultisigSignersFile: signer keys do not match: " <>
      P.show (Map.keys signaturesX, Map.keys signaturesY)
    | publicKeysX /= publicKeysY =
      error $
      "MultisigSignersFile: signer public keys do not match: " <>
      P.show (publicKeysX, publicKeysY)
    | otherwise =
      fileX
        {signatures = Map.unionWithKey appendSignatures signaturesX signaturesY}
    where
      contractNameX = (contractName :: MultisigSignersFile key -> String) fileX
      contractNameY = (contractName :: MultisigSignersFile key -> String) fileY
      contractAddressX = (contractAddress :: MultisigSignersFile key -> Address) fileX
      contractAddressY = (contractAddress :: MultisigSignersFile key -> Address) fileY
      counterX = (counter :: MultisigSignersFile key -> Natural) fileX
      counterY = (counter :: MultisigSignersFile key -> Natural) fileY
      contractParameterX = contractParameter fileX
      contractParameterY = contractParameter fileY
      publicKeysX = publicKeys fileX
      publicKeysY = publicKeys fileY
      signaturesX = signatures fileX
      signaturesY = signatures fileY
      appendSignatures publicKey mSignatureX mSignatureY =
        case mSignatureX of
          Nothing -> mSignatureY
          Just signatureX ->
            case mSignatureY of
              Nothing -> Just signatureX
              Just signatureY ->
                if signatureX == signatureY
                  then Just signatureX
                  else error $
                       "MultisigSignersFile: key: " <>
                       P.show publicKey <>
                       " has duplicate signatures: " <>
                       P.show (signatureX, signatureY)

-- | Make a `MultisigSignersFile` from a contract name,
-- the current counter, a list of the signers' keys,
-- and a contract parameter
makeMultisigSignersFile :: forall key. G.IsKey key
  => String
  -> Address
  -> Natural
  -> [G.Public key]
  -> Either (Natural, [G.Public key]) SomeContractParam
  -> MultisigSignersFile key
makeMultisigSignersFile contractName contractAddress counter signerKeys contractParameter =
  MultisigSignersFile Proxy contractName contractAddress counter contractParameter signerKeys signatures
  where
    signatures = Map.fromList . fmap (, Nothing) $ signerKeys

-- | Write a `MultisigSignersFile` to the path generated by the contract name,
-- counter, and base-58-encoded `Crypto.sha256` hash of the parameters
writeMultisigSignersFile ::
     MultisigSignersFile key
  -> IO ()
writeMultisigSignersFile multisigSignersFile@MultisigSignersFile{..} = do
  putStrLn $ "Writing parameter to file: " <> show filePath
  BL.writeFile filePath $ encodePretty multisigSignersFile
  where
    filePath :: String
    filePath =
      mconcat [contractName, "_", show counter, "_", paramHash, ".json"]

    paramHash :: String
    paramHash =
      T.unpack . Crypto.encodeBase58Check .
      Crypto.sha256 . BL.toStrict . Binary.runPut . Binary.put . show $
      toJSON contractParameter


-- | Read and parse a `MultisigSignersFile`
readMultisigSignersFile :: G.IsKey key => FilePath -> ExceptT String IO (MultisigSignersFile key)
readMultisigSignersFile filePath = do
  lift $ putStrLn filePath
  lift $ canonicalizePath filePath >>= putStrLn
  fileExists <- lift $ canonicalizePath filePath >>= doesFileExist
  if P.not fileExists
     then throwError "file does not exist"
     else do
       -- tt <- lift $ TL.readFile filePath
       -- lift $ putStrLn tt
       lift $ getCurrentDirectory >>= putStrLn
       lift $ getCurrentDirectory >>= listDirectory >>= P.mapM_ putStrLn
       undefined
       lift (BL.readFile filePath) >>= either throwError return . eitherDecode

multisigSignFile :: forall key. G.IsKey key
  => Proxy key
  -> SecretKey
  -> G.SomePublicKey
  -> FilePath
  -> ExceptT String IO ()
multisigSignFile keyProxy secretKey somePublicKey multisigFile =
  case somePublicKey of
    G.SomePublicKey (keyProxy' :: Proxy key') publicKey -> do
      multisigSignersFile <- readMultisigSignersFile @key multisigFile
      case eqT @key @key' of
        Nothing ->
          error . T.pack $
          "MultisigSignersFile uses keys of type " <>
          show (typeRep keyProxy) <>
          ", but received a key of type: " <> show (typeRep keyProxy')
        Just Refl ->
          lift $ writeMultisigSignersFile $
            signMultisigSignersFile @key publicKey secretKey multisigSignersFile

-- | Read a `NonEmpty` list of `MultisigSignersFile`s and concatenate them
readConcatMultisigSignersFiles :: forall key. G.IsKey key => Proxy key -> NonEmpty FilePath -> ExceptT String IO ()
-- readConcatMultisigSignersFiles keyProxy = do
readConcatMultisigSignersFiles _ ~(path :| paths) = do
  readMultisigSignersFile @key "GenericMultisigContract223_0_9YfhzjTTDiXqiNB9KeuQLYUQNX9oCkTAtfEVM7JYZNeHm1vHM.json" >>= lift . print . encode
  multisigSignersFile <- readMultisigSignersFile @key path
  multisigSignersFile `seq` return ()
  multisigSignersFile' <- foldM
    (\ !memo !path' -> (<>) memo <$> readMultisigSignersFile @key path')
    multisigSignersFile
    paths
  lift . putStrLn $ renderMultisigSignersFile multisigSignersFile'

signatureList :: MultisigSignersFile key -> [Maybe (G.Sig key)]
signatureList MultisigSignersFile {..} =
  (\k -> join $
    either (const Nothing) Just <$>
    Map.findWithDefault
      (error $ "Public key not found: " <> P.show k)
      k
      signatures) <$>
  publicKeys

-- | Sign the parameters in a `MultisigSignersFile`
signMultisigSignersFile :: forall key.
     G.Public key -> SecretKey -> MultisigSignersFile key -> MultisigSignersFile key
signMultisigSignersFile publicKey secretKey multisigSignersFile@MultisigSignersFile {..} =
  case contractParameter of
    Left changeKeyParams' ->
      signMainParams
        (G.ChangeKeys changeKeyParams' :: G.GenericMultisigAction key ())
    Right someContractParam' ->
      fromSomeContractParam someContractParam' $ \contractParam ->
        signMainParams $ G.Operation contractParam
  where
    signMainParams ::
         ( Typeable (ToT a)
         , SingI (ToT a)
         , IsoValue a
         , HasNoOp (ToT a)
         , HasNoBigMap (ToT a)
         )
      => G.GenericMultisigAction key a
      -> MultisigSignersFile key
    signMainParams action' =
      multisigSignersFile
        {signatures = Map.insert publicKey (Just signature) signatures}
      where
        existingSignature :: G.PartialSig key
        existingSignature =
          case join $ Map.lookup publicKey signatures of
            Nothing -> G.partialSig @key
            Just eSignature ->
              case eSignature of
                Left partialSignature -> partialSignature
                Right _ ->
                  error "signMultisigSignersFile: expected partial signature but got complete signature"

        signatureAttempt :: G.PartialSig key
        signatureAttempt =
          either (error . T.pack) id $
          G.signWithKey @key secretKey packedValue existingSignature

        signature :: Either (G.PartialSig key) (G.Sig key)
        signature =
          bimap (const signatureAttempt) id $
          G.completeSig @key signatureAttempt

        packedValue =
          packValue' .
          toVal $
          -- G.MainParameter . (, signatureList multisigSignersFile) . (counter, ) $
          (contractAddress, (counter, action'))

-- | Convert to a `Value`, untype, and render
showValue :: (IsoValue t, SingI (ToT t), HasNoOp (ToT t)) => t -> String
showValue = show . renderDoc doesntNeedParens . untypeValue . toVal

-- | Render a `MultisigSignersFile` as a Michelson parameter
renderMultisigSignersFile :: forall key. G.IsKey key => MultisigSignersFile key -> String
renderMultisigSignersFile multisigSignersFile@MultisigSignersFile {..} =
  case contractParameter of
    Left changeKeysParam ->
      makeMainParameter $
      (G.ChangeKeys changeKeysParam :: G.GenericMultisigAction key ())
    Right (SomeContractParam xs (_, _) (Dict, Dict)) -> makeMainParameter' xs
  where
    sortedSignatures :: [Maybe (G.Sig key)]
    sortedSignatures = signatureList multisigSignersFile

    makeMainParameter ::
         (IsoValue a, Typeable (ToT a), SingI (ToT a), HasNoOp (ToT a))
      => G.GenericMultisigAction key a
      -> String
    makeMainParameter param =
      showValue $ G.MainParameter ((counter, param), sortedSignatures)

    makeMainParameter' ::
         forall t'. (Typeable t', SingI t', HasNoOp t')
      => Value t'
      -> String
    makeMainParameter' =
      makeMainParameter @(Value t') . G.Operation .
        (fromVal :: Value (ToT (Value t')) -> Value t')


