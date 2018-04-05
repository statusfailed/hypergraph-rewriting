module Rewriting where

import Control.Monad
import Control.Monad.Logic

import Control.Monad.Reader.Class

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))

import Data.List
import Data.Foldable

import Data.Set (Set(..))
import qualified Data.Set as Set

import Types
import Util (bsum)

-- | Is node v in the domain of a 'Hyperedge'?
inDomain :: Eq v => v -> Hyperedge v -> Bool
inDomain v = elem v . dom

-- | Is node v in the codomain of a hyperedge.
inCodomain :: Eq v => v -> Hyperedge v -> Bool
inCodomain v = elem v . cod


-- Immediate neighbours of a node
neighbours :: Eq v => Hypergraph v v -> v -> [v]
neighbours (Hypergraph vs es) v = nodes where
  edges = filter (inDomain v) es
  nodes = nub <$> cod =<< edges

-- List of nodes reachable from v
--
-- NOTE: this can be sped up by ignoring hyper-edges that have already been
-- encountered.
reachable :: (MonadLogic m, Ord v) => Hypergraph v v -> [v] -> m v
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
convex' :: Ord v => Hypergraph v v -> [v] -> [v]
convex' g vs = b
  where
    subgraph = Set.fromList vs
    context  = Set.difference (Set.fromList (nodes g)) subgraph

    -- Context nodes reachable from subgraph
    a = filter (flip Set.member context) . observeAll . reachable g . toList $ subgraph

    -- Subgraph nodes reachable from a (must be empty!)
    b = filter (flip Set.member subgraph) . observeAll . reachable g . toList $ a

-- | Convexity predicate
convex :: Ord v => Hypergraph v v -> [v] -> Bool
convex g vs = null (convex' g vs)
