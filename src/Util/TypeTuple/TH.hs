{-# LANGUAGE QuasiQuotes #-}

-- | Template haskell generator for 'RecFromTuple', in a separate module
-- because of staging restrictions.
module Util.TypeTuple.TH
  ( deriveRecFromTuple
  ) where

import qualified Data.Kind as Kind
import Data.Vinyl.Core (Rec(..))
import qualified Language.Haskell.TH as TH

import Util.TypeTuple.Class

-- | Produce 'RecFromTuple' instance for tuple of the given length.
deriveRecFromTuple :: Word -> TH.Q [TH.Dec]
deriveRecFromTuple (fromIntegral -> n) = do
  fVar <- TH.VarT <$> TH.newName "f"
  tyVars <- replicateM n $ TH.VarT <$> TH.newName "x"

  let consTy ty lty = TH.promotedConsT `TH.appT` pure ty `TH.appT` lty
  let tyList = foldr consTy TH.promotedNilT tyVars

  let tupleConsTy acc ty = acc `TH.appT` (pure fVar `TH.appT` pure ty)
  let tyTuple = foldl tupleConsTy (TH.tupleT n) tyVars

  vars <- replicateM n $ TH.newName "a"
  let tyPat = pure . TH.TupP $ map TH.VarP vars

  let consRec var acc = [e|(:&)|] `TH.appE` TH.varE var `TH.appE` acc
  let recRes = foldr consRec [e|RNil|] vars

  [d| instance RecFromTuple (Rec ($(pure fVar) :: u -> Kind.Type) $tyList) where
        type IsoRecTuple (Rec $(pure fVar) $tyList) = $tyTuple
        recFromTuple $tyPat = $recRes
    |]
