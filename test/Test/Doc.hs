{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

-- | Tests on automatic documentation generation.

module Test.Doc
  ( test_General_doc_scan
  , test_Dependencies_loops
  ) where

import qualified Data.Map as Map
import Test.HUnit ((@?=))
import Test.QuickCheck (total)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified Lorentz as L
import Michelson.Typed

-- General tests on contract doc scan
----------------------------------------------------------------------------

newtype MyType = MyType Integer
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance TypeHasDoc MyType where
  typeDocMdDescription = ""

test_General_doc_scan :: [TestTree]
test_General_doc_scan =
  [ testCase "Type declaration is found" $
      let contract = L.doc $ DType (Proxy @MyType)
          doc = L.buildLorentzDoc contract
          mTypeDecls = Map.lookup (docItemPosition @DType) (cdDefinitions doc)
         --- Type declarations should include 'MyType' and 'Integer'
      in fmap length mTypeDecls @?= Just 2
  ]

-- Test on loops on dependency graph of doc items
----------------------------------------------------------------------------

-- | Type, documentation for which somehow depends on itself.
newtype MyLoopedType = MyLoopedType Integer
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance TypeHasDoc MyLoopedType where
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @MyLoopedType)]
  typeDocMdDescription = ""

newtype MyMutuallyDependentType1 = MyMutuallyDependentType1 Integer
  deriving stock (Generic)
  deriving anyclass (IsoValue)

newtype MyMutuallyDependentType2 = MyMutuallyDependentType2 Integer
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance TypeHasDoc MyMutuallyDependentType1 where
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @MyMutuallyDependentType2)]
  typeDocMdDescription = ""

instance TypeHasDoc MyMutuallyDependentType2 where
  typeDocDependencies _ = [SomeTypeWithDoc (Proxy @MyMutuallyDependentType1)]
  typeDocMdDescription = ""

test_Dependencies_loops :: [TestTree]
test_Dependencies_loops =
  [ testProperty "Type depending on itself" $
      let contract = L.doc $ DType (Proxy @MyLoopedType)
      in total . contractDocToMarkdown $ L.buildLorentzDoc contract
  , testProperty "Mutually dependent types" $
      let contract = L.doc $ DType (Proxy @MyMutuallyDependentType1)
      in total . contractDocToMarkdown $ L.buildLorentzDoc contract
  ]
