-- | This module essentially replaces the default Prelude with Universum.
--
-- It works because we are using the 'base-noprelude' package instead of 'base'.

module Prelude
       ( module Universum
       ) where

import Universum hiding (Key, Type, Val, readFile, writeFile)
