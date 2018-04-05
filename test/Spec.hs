{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
{-import Test.Tasty.SmallCheck as SC-}
{-import Test.Tasty.QuickCheck as QC-}
import Test.Tasty.HUnit

import Control.Monad
import Control.Monad.Logic

import Data.List (sort, inits, tails)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Types
import Rewriting (neighbours, convex, reachable)
import Match (proposeNodeMatch, proposeEdgeMatchesFor)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testRewritingFunctions
  , testMatchingFunctions
  ]

testMatchingFunctions = testGroup "test matching functions"
  [ testCase "matching a graph to itself should propose all nodes" $ assertEqual ""
      (nodes linearGraph)
      (observeAll (proposeNodeMatch linearGraph linearGraph Map.empty))
  , testCase "proposeNodeMatch should ignore nodes in matched" $ assertEqual ""
      [1..7]
      (observeAll (proposeNodeMatch dpoExample dpoExample (Map.singleton 8 8)))
  , testCase "edgeMatchesFor simple graph" $ assertEqual ""
      [[Hyperedge [2] [2,3]]]
      (observeAll $ proposeEdgeMatchesFor contextGraph patternGraph Map.empty 1)
  ]

testRewritingFunctions = testGroup "test rewriting functions" simpleTests

simpleTests = zipWith f [1..] $
  [ sort (neighbours testGraph 1) == [1,3,4]
  , sort (neighbours testGraph 2) == [1]
  , sort (neighbours testGraph 3) == [3,4]
  , sort (reachable testGraph [1]) == [1,3,4]
  , sort (observeAll $ reachable linearGraph [8]) == []
  , sort (observeAll $ reachable linearGraph [1..8]) == [2..8]
  , all (convex linearGraph . pure) [1..8]
  , all (convex linearGraph) $ subseqs [1..8]
  , convex dpoExample dpoSubgraph
  , not $ convex dpoExample (filter (/= 3) dpoSubgraph)
  ]
  where
    f i result =
      let msg = ("simpleTests " ++ show i)
      in  testCase msg $ assertBool msg result


----- Test data ---------

-- Should propose [1,2] for [1] and [2,3] for [2].
patternGraph :: Hypergraph Int Int
patternGraph = mkGraph [1] [ Hyperedge [1] [1,2] ]

contextGraph :: Hypergraph Int Int
contextGraph = mkGraph v e where
  v = [1..3]
  e = [ Hyperedge [1] [1,2], Hyperedge [2] [2,3] ]

-- A simple test graph
testGraph = mkGraph v e where
  v = [1..4]
  e =
    [ Hyperedge [1, 3] [3, 4]
    , Hyperedge [1, 2] [1]
    ]

mkLinearGraph :: Int -> Hypergraph Int Int
mkLinearGraph n = mkGraph v e
  where
    v = [1..n]
    e = zipWith (\v1 v2 -> Hyperedge [v1] [v2]) v (drop 1 v)

-- A graph like 1 --> 2 --> 3 --> ... --> 8
linearGraph :: Hypergraph Int Int
linearGraph = mkLinearGraph 8

-- DPO example from http://www.cas.mcmaster.ca/~kahl/CAS701/2007/P/DPO.pdf
dpoExample = mkGraph v e where
  v = [1..7]
  e =
    [ Hyperedge [1] [2, 3, 6]
    , Hyperedge [2] [7]
    , Hyperedge [3] [2]
    , Hyperedge [6] [7]
    ]
dpoSubgraph = [1,2,3]

------ Utilities -----

-- contiguous subsequences
subseqs :: [a] -> [[a]]
subseqs xs = filter (not . null) $ inits xs >>= tails
