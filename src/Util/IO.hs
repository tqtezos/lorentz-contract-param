module Util.IO
  ( readFileUtf8
  , writeFileUtf8
  , withEncoding
  , hSetTranslit
  ) where

import Data.Text.IO (hGetContents)
import GHC.IO.Encoding (textEncodingName)
import System.IO (TextEncoding, hGetEncoding, hSetBinaryMode, hSetEncoding, mkTextEncoding, utf8)

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 name =
  openFile name ReadMode >>= \h -> hSetEncoding h utf8 >> hGetContents h

writeFileUtf8 :: Print text => FilePath -> text -> IO ()
writeFileUtf8 name txt =
  withFile name WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h txt

withEncoding :: Handle -> TextEncoding -> IO () -> IO ()
withEncoding handle encoding action = do
  mbInitialEncoding <- hGetEncoding handle
  bracket
    (hSetEncoding handle encoding)
    (\_ -> maybe (hSetBinaryMode handle True) (hSetEncoding handle) mbInitialEncoding)
    (\_ -> action)

-- This function was copied (with slight modifications) from
-- <https://gitlab.haskell.org/ghc/ghc/blob/7105fb66a7bacf822f7f23028136f89ff5737d0e/libraries/ghc-boot/GHC/HandleEncoding.hs>
--
-- © 2002 The University Court of the University of Glasgow
-- (original license: LicenseRef-BSD-3-Clause-TheUniversityCourtOfTheUniversityOfGlasgow)
-- | Change the character encoding of the given Handle to transliterate
-- on unsupported characters instead of throwing an exception.
hSetTranslit :: Handle -> IO ()
hSetTranslit h = do
    menc <- hGetEncoding h
    case fmap textEncodingName menc of
        Just name | '/' `notElem` name -> do
            enc' <- mkTextEncoding $ name ++ "//TRANSLIT"
            hSetEncoding h enc'
        _ -> pass
