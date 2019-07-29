{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Doc
  ( doc
  , docGroup
  , buildLorentzDoc
  , contractName

    -- * Re-exports
  , DocItem (..)
  , TypeHasDoc (..)
  , PolyTypeHasDocC
  , SomeTypeWithDoc (..)

  , DDescription (..)
  , DError (..)
  , DType (..)
  , contractDocToMarkdown

  , HaveCommonTypeCtor
  , IsHomomorphic
  , genericTypeDocDependencies
  , customTypeDocMdReference
  , homomorphicTypeDocMdReference
  , poly1TypeDocMdReference
  , poly2TypeDocMdReference
  , homomorphicTypeDocHaskellRep
  , concreteTypeDocHaskellRep
  , haskellRepNoFields
  , haskellRepStripFieldPrefix
  , homomorphicTypeDocMichelsonRep
  , concreteTypeDocMichelsonRep
  ) where

import Data.Singletons (demote)

import Lorentz.Base
import Michelson.Typed

-- | Put a document item.
doc :: DocItem di => di -> s :-> s
doc = I . Ext . DOC_ITEM . SomeDocItem

-- | Group documentation built in the given piece of code
-- into block dedicated to one thing, e.g. to one entry point.
docGroup :: DocGrouping -> (inp :-> out) -> (inp :-> out)
docGroup gr (I i) = I $ DocGroup gr i

-- | Give a name to given contract. Apply it to the whole contract code.
contractName :: Text -> (inp :-> out) -> (inp :-> out)
contractName name = docGroup (SomeDocItem . DName name)

buildLorentzDoc :: inp :-> out -> ContractDoc
buildLorentzDoc (I code) = buildInstrDoc code

instance Each [Typeable, TypeHasDoc] [i, o] => TypeHasDoc (Lambda i o) where
  typeDocName _ = "Lambda"
  typeDocMdReference tp =
    customTypeDocMdReference
      ("Lambda", DType tp)
      [ DType (Proxy @i)
      , DType (Proxy @o)
      ]
  typeDocMdDescription =
    "`Lambda i o` stands for a sequence of instructions which accepts stack \
    \of type `[i]` and returns stack of type `[o]`."
  typeDocDependencies _ =
    [ SomeTypeWithDoc (Proxy @i), SomeTypeWithDoc (Proxy @o)
    , SomeTypeWithDoc (Proxy @Integer), SomeTypeWithDoc (Proxy @Natural)
    ]
  typeDocHaskellRep _ = Nothing
  typeDocMichelsonRep _ =
    ( Just "Lambda Integer Natural"
    , demote @(ToT (Lambda Integer Natural))
    )
