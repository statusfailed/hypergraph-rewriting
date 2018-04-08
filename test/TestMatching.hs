{-# LANGUAGE OverloadedStrings #-}
module TestMatching where

import Test.Tasty
{-import Test.Tasty.SmallCheck as SC-}
{-import Test.Tasty.QuickCheck as QC-}
import Test.Tasty.HUnit

import Control.Monad
import Control.Monad.Logic

import Data.List (sort, inits, tails)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Foldable

import SMC

main = runLogicState (match testGraph testPattern) tasks
  where tasks = map V [0..4] ++ map E [0..1]

testMatching = testGroup "test matching" unitTests

-- | Utility to make a predicat about returned matches.
testMatches
  :: (Ord e, Ord v)
  => String -> ([Matching] -> Bool) -> Hypergraph v e -> Hypergraph v e -> TestTree
testMatches msg f graph pattern = testCase msg $ assertBool msg result
  where
    result = f . fmap fst $ runLogicState (match graph pattern) tasks
    tasks  =
      map V (nodeNames pattern) ++
      map E (edgeNames pattern)

unitTests :: [TestTree]
unitTests =
  [ testMatches "testGraph has matches" (not . null) testGraph testPattern
  ]

----- Unit test example data -----

testPattern = mkGraph v e where
  ixs = [0..4]
  name = ('V':) . show
  v = map name ixs
  e =
    [ mkEdge () [0] [1,2]
    , mkEdge () [0] [3,4]
    ]

testGraph = mkGraph v e where
  ixs = [0..5]
  name = ('V':) . show
  v = map name ixs
  e =
    [ mkEdge () [0] [1,2]
    , mkEdge () [0] [5,0]
    , mkEdge () [0] [3,4]
    ]

