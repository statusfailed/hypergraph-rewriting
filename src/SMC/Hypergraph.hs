{-# LANGUAGE DeriveGeneric #-}
module SMC.Hypergraph where

import GHC.Generics

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Logic
import Control.Monad.State

import Data.Maybe (catMaybes)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))

import Data.List hiding ((\\))
import Data.Foldable

import Data.Set (Set(..), (\\))
import qualified Data.Set as Set

import Data.Vector (Vector(..), (!))
import qualified Data.Vector as Vector

import SMC.Util (bsum)

type LogicState s = StateT s Logic

runLogicState :: LogicState s a -> s -> [(a, s)]
runLogicState m s = observeAll (runStateT m s)

-- | Directed hyperedges, with labels of type 'a'
data Hyperedge a = Hyperedge
  { val :: a
  , dom :: Vector Int
  , cod :: Vector Int
  } deriving(Eq, Ord, Read, Show, Generic)

instance Functor Hyperedge where
  fmap f (Hyperedge a d c) = Hyperedge (f a) d c


-- | Hypergraph with nodes labeled with type v, edges with type e.
data Hypergraph v e = Hypergraph
  { nodes :: Vector v
  , edges :: Vector (Hyperedge e)
  } deriving(Eq, Ord, Read, Show, Generic)

-- Functor on edge type
instance Functor (Hypergraph v) where
  fmap f (Hypergraph n e) = Hypergraph n (fmap (fmap f) e)

-- | Disjoint union of Vertices and Edges.
data VE = V Int | E Int
  deriving(Eq, Ord, Read, Show, Generic)

-- TODO: smart constructor; check vertexes are a superset of those referenced in
-- edges.
mkGraph :: [v] -> [Hyperedge e] -> Hypergraph v e
mkGraph vs es = Hypergraph (Vector.fromList vs) (Vector.fromList es)

mkEdge :: a -> [Int] -> [Int] -> Hyperedge a
mkEdge val dom cod = Hyperedge val (Vector.fromList dom) (Vector.fromList cod)

-- | Indexes of all nodes in a graph
nodeNames :: Hypergraph v e -> [Int]
nodeNames g
  | Vector.null (nodes g) = []
  | otherwise = [0 .. Vector.length (nodes g) - 1]

-- | Indexes of all edges in a graph
edgeNames :: Hypergraph v e -> [Int]
edgeNames g
  | Vector.null (edges g) = []
  | otherwise = [0 .. Vector.length (edges g) - 1]

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

-- | Generalised neighbours', finds nodes and edges neighbouring an initial set
-- of nodes and edges.
neighboursVE :: Hypergraph v e -> [VE] -> [VE]
neighboursVE g ves = ves >>= f
  where
    f (V v) =
      fmap (E . fst) . filter (inDomain v . snd) . zipWith (,) [0..] . toList $ edges g
    f (E e) = fmap V . toList . cod $ edges g ! e

-- | Edges and Vertices reachable from an initial set of edges and vertices.
-- An edge is not counted as "reachable" from itself, unless there is a path
-- back to it.
reachableVE :: Hypergraph v e -> [VE] -> Set VE
reachableVE g ves = go g Set.empty (neighboursVE g ves) where
  go :: Hypergraph v e -> Set VE -> [VE] -> Set VE
  go g visited current
    | null current = visited
    | otherwise =
        let visited' = Set.union visited $ Set.fromList current
            current' = filter (flip Set.notMember visited) (neighboursVE g current)
        in go g visited' current'

convexVE' :: Hypergraph v e -> [VE] -> Set VE
convexVE' g ves = c where
  a = reachableVE g ves
  b = reachableVE g (toList a)
  c = Set.filter (flip Set.member $ Set.fromList ves) b

convexVE :: Hypergraph v e -> [VE] -> Bool
convexVE g = Set.null . convexVE' g

-- Is a subgraph convex?
-- TODO BUG: example 5.3 in paper; *no nodes* in context! only edges. Need to
-- check both!
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

------------

-- | Free nodes on the left and right
-- i.e. only those nodes which don't appear in both a domain and codomain of some edges.
freeNodes :: Hypergraph v e -> (Set Int, Set Int)
freeNodes g@(Hypergraph ns es) = (free \\ cs, free \\ ds)
  where
    f g = Set.fromList . toList $ es >>= g
    ds = f dom
    cs = f cod
    ns = Set.fromList (nodeNames g)
    free = ns \\ Set.intersection ds cs

-- | All nodes in the boundary of a graph
boundary :: Hypergraph v e -> Set Int
boundary = uncurry Set.union . freeNodes

-- | Rewrite the nodes in an edge, allowing for failure.
remapEdgeM :: Monad m => (Int -> m Int) -> Hyperedge e -> m (Hyperedge e)
remapEdgeM f (Hyperedge v d c) = liftM2 (Hyperedge v) (Vector.mapM f d) (Vector.mapM f c)

remapEdge :: (Int -> Int) -> Hyperedge e -> Hyperedge e
remapEdge f = runIdentity . remapEdgeM (return . f)

-- | Make a hypergraph from two disconnected subgraphs, with no connecting
-- parts.
--
-- Renames all nodes in second graph by (+n), where n is num nodes in 1st graph.
-- TODO: test -
disjoint :: Hypergraph v e -> Hypergraph v e -> Hypergraph v e
disjoint h g = Hypergraph (nodes h Vector.++ nodes g) (edges h Vector.++ newEdgesG)
  where
    n = Vector.length $ nodes h
    newEdgesG = Vector.map (remapEdge (+n)) (edges g)

-- | Removes nodes and edges in a graph.
--
-- If a retained edge would reference a removed node, remove it as well.
--
-- If a node has index i, then nodes with indexes j < i are not renumbered.
cutWhere :: (VE -> Bool) -> Hypergraph v e -> Hypergraph v e
cutWhere f g = Hypergraph newNodes $ Vector.fromList newEdges
  where
    -- Remove undesired nodes + edges
    keepNodes = filter (not . f . V) (nodeNames g) -- note: nodeNames is a sorted list!
    keepEdges = filter (not . f . E) (edgeNames g)

    -- map old node indexes to new ones, for reconnecting edges.
    nodeRemap = Map.fromList $ zip keepNodes [0..] -- rename nodes

    -- new node vector
    newNodes = Vector.fromList $ fmap (nodes g !) keepNodes

    -- new edge vector: look up edges, renumber domains/codomains, and remove those which
    -- reference missing nodes.
    newEdges = catMaybes . fmap (remapEdgeM (flip Map.lookup nodeRemap))
             $ fmap (edges g!) keepEdges

-- | mergeNodes x y removes node y from the graph, and replaces all references
-- to it with x.
--
-- NOTE: renumbers nodes with index > y
mergePair :: Int -> Int -> Hypergraph v e -> Hypergraph v e
mergePair i j (Hypergraph n e) =
  cutWhere (== V j) . Hypergraph n . fmap (remapEdge f) $ e
  where
    f x = if x == j then i else x

-- Make nodes equal
mergeNodes :: Map Int Int -> Hypergraph v e -> Hypergraph v e
mergeNodes m (Hypergraph n e) =
  cutWhere isCut . Hypergraph n . fmap (remapEdge rename) $ e
  where
    -- rewrite if present in map
    rename x = maybe x id (Map.lookup x m)
    -- remove from graph if that node was rewritten
    isCut x = case x of
      V i -> maybe False (/=i) (Map.lookup i m)
      E i -> False

-- tests
--  let g, h be graphs
--  in  g == cut (allOf h) (smoosh g h)


-- | Is this a valid hypergraph? (for SMC rewriting)
-- returns True if:
--    * No node appears more than twice in all edges in the graph.
isValid :: Hypergraph v e -> Bool
isValid (Hypergraph _ e) = Map.null $ Map.filter (> 2) both
  where
    countFreqs = foldr $ Map.alter (Just . maybe 1 (+1))
    doms = countFreqs Map.empty (fmap dom e)
    both = countFreqs doms (fmap cod e)
