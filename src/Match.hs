module Match where

import Control.Monad
import Control.Monad.Logic

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Types
import Util (bsum)

-- Idea:
--   0. Choose an arbitrary node from the pattern
--   1. Propose all nodes in context as a match
--   2. Propose all edges for this node, search
{-match :: MonadLogic m => Hypergraph v v -> Hypergraph v v -> m (Hypergraph v v)-}
{-match graph [] = return (Hypergraph [] []) -- empty pattern matches in all graphs-}
{-match (Hypergraph [] _) pattern = graph    -- empty graph   matches no patterns-}
{-match graph pattern = do-}
  {-ctxNode <- proposeNodeMatch graph pattern state patNode-}
  {-ctxEdge <- proposeEdgeMatch graph pattern state patEdge-}
  {-where start = pattern (nodes graph)-}

-- | Auxiliary function for 'matched'.
--
-- State: Map vmatch vgraph
-- vmatch: id of node in "match" graph
-- vgraph: id of node in graph
-- Procedure:
--  0. If all nodes matched, halt, return nodes.
--  1. Pick a node v <- not in vmatch
--  2. Pick all edges, e <- ematch
--    2.5: if any e not in graph, fail
--  3. Add v to matched
--
{-go-}
  {-:: (MonadLogic m, Ord v)-}
  {-=> Hypergraph v v -> Hypergraph v v -> Map v v -> m (Map v v)-}
{-go graph pattern matched-}
  {-| length (nodes pattern) == Map.size matched = return matched-}
  {-| otherwise = do-}
      {-v  <- proposeNodeMatch graph pattern matched-}
      {-es <- proposeEdgeMatchesFor graph pattern matched v-}

-- TODO: identify nodes and edges by their index. Currently difficult to tell
-- if an edge has been used already!
proposeEdgeMatchesFor
  :: (MonadLogic m, Ord v)
  => Hypergraph v v -- ^ Graph to match in
  -> Hypergraph v v -- ^ Pattern to match
  -> Map v v -- ^ Already-matched nodes, and their corresponding vertexes in graph
  -> v -- ^ Node to propose edge sets for
  -> m [Hyperedge v] -- ^ Candidate edge-set
proposeEdgeMatchesFor graph pattern matched node = return []

proposeNodeMatch
  :: (MonadLogic m, Ord v)
  => Hypergraph v v -- ^ Graph to match in
  -> Hypergraph v v -- ^ Pattern to match
  -> Map v v -- ^ Already-matched nodes, and their corresponding vertexes in graph
  -> m v -- ^ Candidate node
proposeNodeMatch graph pattern matched =
  bsum . filter (un matched) . nodes $ pattern
  where un m = not . flip Map.member m
