module Main
  ( main
  ) where

import Test.Tasty (defaultMainWithIngredients)

import Util.Test.Ingredients (ourIngredients)

import Tree (tests)

main :: IO ()
-- main = putStrLn ("\n\nNo tests implemented\n\n" :: String)
main = tests >>= defaultMainWithIngredients ourIngredients
