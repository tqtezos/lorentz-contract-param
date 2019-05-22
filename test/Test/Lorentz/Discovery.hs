{-# LANGUAGE QuasiQuotes #-}

-- | Tests for contracts discovery.

module Test.Lorentz.Discovery
  ( test_Export_list_parse
  ) where

import Test.HUnit (assertFailure, (@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Text.InterpolatedString.QM (qnb)
import Text.Megaparsec (errorBundlePretty, runParser)

import Lorentz.Discover

test_Export_list_parse :: [TestTree]
test_Export_list_parse =
  [ testCase "Empty" $
    [qnb| module MyModule () where
        |] `shouldParseTo` []

  , testCase "Empty spaced" $
    [qnb| module MyModule (  ) where
        |] `shouldParseTo` []

  , testCase "Contracts extracted fine" $
      [qnb| module MyModule
              ( contract_Some
              , contract_one_another
              ) where
      |]
      `shouldParseTo`
       [ ExportedContractInfo
         { eciModuleName = "MyModule"
         , eciContractDecl = ExportedContractDecl
           { ecdName = "Some"
           , ecdVar = "contract_Some"
           }
         }
       , ExportedContractInfo
         { eciModuleName = "MyModule"
         , eciContractDecl = ExportedContractDecl
           { ecdName = "one another"
           , ecdVar = "contract_one_another"
           }
         }
       ]

  , testCase "Bad export entries are ignored" $
      [qnb| module MyModule
              ( not_a_contract
              , SomeType (..)
              , SomethingElse ( Ctor, getter )
              , SomethingElse2
                 ( Ctor2  -- constructor
                 , getter2  {- getter -})
              , contract
              , contract_Some
              ) where
      |]
      `shouldParseTo`
       [ ExportedContractInfo
         { eciModuleName = "MyModule"
         , eciContractDecl = ExportedContractDecl
           { ecdName = "Some"
           , ecdVar = "contract_Some"
           }
         }
       ]

  , testCase "Annotations and comments" $
      [qnb| {-# PRAGMA #-}
            -- Comment
            -- | Description
            {- Another comment
            -}
            module MyModule
              ( contract_Some  -- Should be exported

                -- ** And these should not
              , contract
              ) where
      |]
      `shouldParseTo`
       [ ExportedContractInfo
         { eciModuleName = "MyModule"
         , eciContractDecl = ExportedContractDecl
           { ecdName = "Some"
           , ecdVar = "contract_Some"
           }
         }
       ]

  ]
  where
    shouldParseTo code exports =
      case runParser haskellExportsParser "" code of
        Left err -> assertFailure $ errorBundlePretty err
        Right x -> x @?= exports
