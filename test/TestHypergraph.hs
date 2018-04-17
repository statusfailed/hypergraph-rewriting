{-# LANGUAGE OverloadedStrings #-}
module TestHypergraph where

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
  ( Hypergraph(..), Hyperedge(..), VE(..), mkEdge, mkGraph
  , neighbours, neighboursVE, reachableVE, convex, reachable
  )

testHypergraphFunctions = testGroup "test hypergraph functions" simpleTests

simpleTests = zipWith f [1..] $
  [ sort (neighbours testGraph 0) == [0,2,3]
  , sort (neighbours testGraph 1) == [0]
  , sort (neighbours testGraph 2) == [2,3]
  , sort (reachable testGraph [0]) == [0,2,3]
  , sort (observeAll $ reachable linearGraph [7]) == []
  , sort (observeAll $ reachable linearGraph [0..7]) == [1..7]
  , all (convex linearGraph . pure) [0..7]
  , all (convex linearGraph) $ subseqs [0..7]
  , convex dpoExample dpoSubgraph
  , not $ convex dpoExample (filter (/= 2) dpoSubgraph)
  , sort (neighboursVE testGraph [V 0]) == fmap E [0,1]
  , sort (neighboursVE testGraph [V 0, E 1]) == sort (V 0 : fmap E [0,1])
  , sort (toList $ reachableVE testGraph [V 0]) == sort [V 0, V 2, V 3, E 0, E 1]
  , sort (toList $ reachableVE testGraph [V 3]) == []
  , sort (toList $ reachableVE contextGraph [V 0]) == [V 0, V 1, V 2, E 0, E 1]
  , sort (toList $ reachableVE contextGraph [V 1]) == [V 1, V 2, E 1]
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
  v = map nodeName [0..2]
  e = [ mkEdge "E0" [0] [0,1], mkEdge "E1" [1] [1,2] ]

-- A simple test graph
testGraph = mkGraph v e where
  v = [0..3]
  e =
    [ mkEdge "E0" [0, 2] [2, 3]
    , mkEdge "E1" [0, 1] [0]
    ]

mkLinearGraph :: Int -> Hypergraph String String
mkLinearGraph n = mkGraph v e
  where
    ixs = [0..n-1]
    v = map nodeName ixs
    e = zipWith (\v1 v2 -> mkEdge ("E" ++ show v1) [v1] [v2]) ixs (drop 1 ixs)

-- A graph like 1 --> 2 --> 3 --> ... --> 8
linearGraph :: Hypergraph String String
linearGraph = mkLinearGraph 8

-- DPO example from http://www.cas.mcmaster.ca/~kahl/CAS701/2007/P/DPO.pdf
dpoExample = mkGraph v e where
  v = map nodeName [0..6]
  e =
    [ mkEdge "E0" [0] [1, 2, 5] -- in pattern
    , mkEdge "E1" [1] [6]       -- in pattern
    , mkEdge "E2" [2] [1]       -- in pattern
    , mkEdge "E3" [5] [6]
    ]

dpoSubgraph = [0,1,2]

dpoSubgraphVE = [V 0, V 1, V 2, E 0]

------ Utilities -----

-- contiguous subsequences
subseqs :: [a] -> [[a]]
subseqs xs = filter (not . null) $ inits xs >>= tails
