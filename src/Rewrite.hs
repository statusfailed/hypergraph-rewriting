module Rewrite where

import Control.Monad
import Control.Monad.Logic

import Control.Monad.Reader.Class

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))

import Data.List
import Data.Foldable

import Data.Set (Set(..))
import qualified Data.Set as Set

import Data.Vector (Vector(..))
import qualified Data.Vector as Vector

import Types
import Util (bsum)

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


-- | Is node v in the domain of a 'Hyperedge'?
inDomain :: Int -> Hyperedge a -> Bool
inDomain i = Vector.elem i . dom

-- | Is node v in the codomain of a hyperedge.
inCodomain :: Int -> Hyperedge a -> Bool
inCodomain i = Vector.elem i . cod


-- Immediate neighbours of a node
neighbours :: Hypergraph v e -> Int -> [Int]
neighbours (Hypergraph vs es) i = nodes where
  edges = filter (inDomain i) (toList es)
  nodes = nub <$> (toList . cod) =<< edges

-- List of nodes reachable from a list of starting vertices
--
-- NOTE: this can be sped up by ignoring hyper-edges that have already been
-- encountered.
reachable :: MonadLogic m => Hypergraph v e -> [Int] -> m Int
reachable g vs = go Set.empty (vs >>= neighbours g) where
  n = length (nodes g)

  go visited current = do
    guard . not . null $ current
    interleave (bsum current) (go vs' cs')
    where
      vs' = Set.union visited (Set.fromList current)
      cs' = filter (not . flip Set.member vs') $ neighbours g =<< current

-- Is a subgraph convex?
--
-- let A be the set of all context nodes reachable from inside the subgraph
-- let B be the set of all subgraph nodes reachable from A
-- convex returns true if B is empty.
--
-- slightly more formally:
--
-- let A be the intersection of context + nodes reachable from subgraph
-- let B be the intersection of subgraph + nodes reachable from A
--
-- This function returns B - the nodes which "break" the convexity property
--
-- TODO: bug with empty graph
convex' :: Hypergraph v e -> [Int] -> [Int]
convex' g vs = b
  where
    n = Vector.length $ nodes g
    subgraph = Set.fromList vs
    context  = Set.difference (Set.fromList [0..n-1]) subgraph

    -- Context nodes reachable from subgraph
    a = filter (flip Set.member context) . observeAll . reachable g . toList $ subgraph

    -- Subgraph nodes reachable from a (must be empty!)
    b = filter (flip Set.member subgraph) . observeAll . reachable g . toList $ a

-- | Convexity predicate
convex :: Ord v => Hypergraph v e -> [Int] -> Bool
convex g vs = null (convex' g vs)
