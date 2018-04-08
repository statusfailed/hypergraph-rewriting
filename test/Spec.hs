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

import Data.Foldable

import SMC.Hypergraph
  ( Hypergraph(..), Hyperedge(..), mkEdge, mkGraph
  , neighbours, convex, reachable
  )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testRewritingFunctions
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

-- Utility for making node names
nodeName :: Int -> String
nodeName i = 'V':show i


-- Should propose [1,2] for [1] and [2,3] for [2].
patternGraph :: Hypergraph String String
patternGraph = mkGraph ["V1", "V2"] [ mkEdge "E1" [1] [1,2] ]

contextGraph :: Hypergraph String String
contextGraph = mkGraph v e where
  v = map nodeName [1..3]
  e = [ mkEdge "E1" [1] [1,2], mkEdge "E2" [2] [2,3] ]

-- A simple test graph
testGraph = mkGraph v e where
  v = [1..4]
  e =
    [ mkEdge "E1" [1, 3] [3, 4]
    , mkEdge "E2" [1, 2] [1]
    ]

mkLinearGraph :: Int -> Hypergraph String String
mkLinearGraph n = mkGraph v e
  where
    ixs = [1..n]
    v = map nodeName ixs
    e = zipWith (\v1 v2 -> mkEdge ("E" ++ show v1) [v1] [v2]) ixs (drop 1 ixs)

-- A graph like 1 --> 2 --> 3 --> ... --> 8
linearGraph :: Hypergraph String String
linearGraph = mkLinearGraph 8

-- DPO example from http://www.cas.mcmaster.ca/~kahl/CAS701/2007/P/DPO.pdf
dpoExample = mkGraph v e where
  ixs = [1..7]
  v = map nodeName [1..7]
  e =
    [ mkEdge "E1" [1] [2, 3, 6]
    , mkEdge "E2" [2] [7]
    , mkEdge "E3" [3] [2]
    , mkEdge "E4" [6] [7]
    ]
dpoSubgraph = [1,2,3]

------ Utilities -----

-- contiguous subsequences
subseqs :: [a] -> [[a]]
subseqs xs = filter (not . null) $ inits xs >>= tails
