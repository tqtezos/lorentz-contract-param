-- | Ingridients that we use in our test suite.

module Util.Test.Ingredients
  ( ourIngredients
  ) where

import Test.Tasty (defaultIngredients)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Runners.AntXML (antXMLRunner)

-- | This is the default set of ingredients extended with the
-- 'antXMLRunner' which is used to generate xml reports for CI.
ourIngredients :: [Ingredient]
ourIngredients = antXMLRunner:defaultIngredients
