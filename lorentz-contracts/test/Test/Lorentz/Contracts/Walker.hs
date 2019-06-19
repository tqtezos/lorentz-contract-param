-- | Module, containing spec to walker contract.

module Test.Lorentz.Contracts.Walker
  ( spec_Walker
  ) where

import Data.Default (def)
import Fmt ((+|), (|+))
import Test.Hspec (Expectation, Spec, expectationFailure, it)

import Lorentz (StorageSkeleton(..), compileLorentz, storeLookup, storePiece)
import Michelson.Test (ContractPropValidator, contractRepeatedProp)
import Michelson.Test.Dummy
import qualified Michelson.Typed as T
import Util.Named ((.!))

import Lorentz.Contracts.Walker

-- | Spec to test walker contract.
spec_Walker :: Spec
spec_Walker = do
  it "Go " $
    walkerProp def [GoRight]
      (\s -> pos (sFields s) == Position 1 0)

  it "Go left" $
    walkerProp def [GoLeft]
      (\s -> pos (sFields s) == Position (-1) 0)

  it "Walk a lot (ADT test)" $
    let commands = mconcat
          [ replicate 1 GoUp
          , replicate 2 GoLeft
          , one $ Boost (#coef1 .! 5, #coef2 .! 0)
          , replicate 3 GoDown
          , replicate 4 GoRight
          , one $ Boost (#coef1 .! 3, #coef2 .! 999)
          ]
    in walkerProp def commands
      (\s -> pos (sFields s) == Position 2 (-2) && power (sFields s) == 8)

  it "Reset" $
    let commands = mconcat
          [ replicate 2 GoUp
          , replicate 2 GoLeft
          , one $ Reset 2
          , replicate 1 GoRight
          ]
    in walkerProp def commands
      (\s -> pos (sFields s) == Position 1 0 && iterId (sFields s) == 2)

  it "Too large boost (`if then else` test)" $
    walkerProp def [Boost (#coef1 .! 999, #coef2 .! 999)]
      (\s -> power (sFields s) == 100)

  it "Power ups work (`Store` access test)" $
    walkerProp
      def{ sMap = mconcat
           [ storePiece #cPowerUps (Position 1 1) (BoostPowerUp 7)
           ]
         }
      [GoRight, GoUp]
      (\s -> power (sFields s) == 7)

  it "Visited cells tracking works (`Store` insert test)" $
    walkerProp
      def
      [GoRight, GoUp]
      $ \s -> and
         [ isJust    $ storeLookup #cVisited (Position 1 1) (sMap s)
         , isNothing $ storeLookup #cVisited (Position 0 0) (sMap s)
         , isJust    $ storeLookup #cVisited (Position 1 0) (sMap s)
         , isNothing $ storeLookup #cVisited (Position 0 1) (sMap s)
         ]

walkerProp :: Storage -> [Parameter] -> (Storage -> Bool) -> Expectation
walkerProp storage params predicate =
  contractRepeatedProp
    (compileLorentz walkerContract)
    (validateStorageSatisfies (predicate . T.fromVal))
    dummyContractEnv params storage

validateStorageSatisfies
  :: (T.Value st -> Bool)
  -> ContractPropValidator st Expectation
validateStorageSatisfies predicate (res, _) = case res of
  Left err -> expectationFailure $ "Interpretation failed: " +| err |+ ""
  Right (_, got) ->
    if predicate got
    then pass
    else expectationFailure $ "Bad resulting storage: " <> show got
