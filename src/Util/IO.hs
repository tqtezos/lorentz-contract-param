module Util.IO
  ( readFileUtf8
  , withEncoding
  ) where

import Data.Text.IO (hGetContents)
import System.IO
  (TextEncoding, hGetEncoding, hSetBinaryMode, hSetEncoding, utf8)

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 name =
  openFile name ReadMode >>= \h -> hSetEncoding h utf8 >> hGetContents h

withEncoding :: Handle -> TextEncoding -> IO () -> IO ()
withEncoding handle encoding action = do
  mbInitialEncoding <- hGetEncoding handle
  bracket
    (hSetEncoding handle encoding)
    (\_ -> maybe (hSetBinaryMode handle True) (hSetEncoding handle) mbInitialEncoding)
    (\_ -> action)
