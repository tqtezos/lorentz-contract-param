module Main
  ( main
  ) where

import Test.Tasty (defaultMainWithIngredients)

import Test.Util.Ingredients (ourIngredients)
import Tree (tests)

main :: IO ()
main = tests >>= defaultMainWithIngredients ourIngredients
