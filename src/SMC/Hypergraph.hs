module SMC.Hypergraph where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.State

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))

import Data.List hiding ((\\))
import Data.Foldable

import Data.Set (Set(..), (\\))
import qualified Data.Set as Set

import Data.Vector (Vector(..))
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
  } deriving(Eq, Ord, Read, Show)

instance Functor Hyperedge where
  fmap f (Hyperedge a d c) = Hyperedge (f a) d c


-- | Hypergraph with nodes labeled with type v, edges with type e.
data Hypergraph v e = Hypergraph
  { nodes :: Vector v
  , edges :: Vector (Hyperedge e)
  } deriving(Eq, Ord, Read, Show)

-- Functor on edge type
instance Functor (Hypergraph v) where
  fmap f (Hypergraph n e) = Hypergraph n (fmap (fmap f) e)


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
