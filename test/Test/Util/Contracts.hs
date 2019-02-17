-- | Utility functions to read sample contracts (for testing).

module Test.Util.Contracts
       ( getIllTypedContracts
       , getWellTypedContracts
       ) where

import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))

getIllTypedContracts :: IO [FilePath]
getIllTypedContracts = getContracts "contracts/ill-typed"

getWellTypedContracts :: IO [FilePath]
getWellTypedContracts = getContracts "contracts"

getContracts :: FilePath -> IO [FilePath]
getContracts dir = mapMaybe convertPath <$> listDirectory dir
  where
    convertPath :: FilePath -> Maybe FilePath
    convertPath fileName
      | (isSuffixOf ".tz" fileName) || (isSuffixOf ".mtz" fileName) =
        Just (dir </> fileName)
      | otherwise = Nothing
