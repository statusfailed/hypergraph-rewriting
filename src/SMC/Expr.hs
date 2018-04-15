{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module SMC.Expr where

import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Catch

import SMC.Hypergraph
import SMC.Match

import Data.Function (on)
import Data.List (sort, groupBy)

import Data.Map (Map(..), (!))
import qualified Data.Map as Map

-- we need stronglyConnComp to renumber nodes when identity or twist terms
-- are encountered.
import Data.Graph (stronglyConnComp, flattenSCC)

data Expr e
  = Id
  | Twist
  | Generator e
  | Seq (Expr e) (Expr e)
  | Par (Expr e) (Expr e)
  deriving(Eq, Ord, Read, Show)

-- | Calculate "type" of expression given a function returning types of generators.
typeOf :: (MonadPlus m) => (e -> m (Int, Int)) -> Expr e -> m (Int, Int)
typeOf f Id = return (1, 1)
typeOf f Twist = return (2, 2)
typeOf f (Generator e) = f e
typeOf f (Seq x y) = do
  (i,j) <- typeOf f x
  (m,n) <- typeOf f y
  guard (j == m)
  return (i, n)
typeOf f (Par x y) = do
  (i,j) <- typeOf f x
  (m,n) <- typeOf f y
  return (i+m, j+n)

data BuildState e = BuildState
  { numVars    :: Int
  , newEdges   :: [Hyperedge e] -- ^ NOTE: this should really be in a WriterT
  , equalities :: [(Int, Int)] -- ^ Which "nodes" are actually the same
  } deriving(Eq, Ord, Read, Show)


-- | Add a new node to the BuildState.
freshNode :: MonadState (BuildState e) m => m Int
freshNode = do
  (BuildState i s eqs) <- get
  put $ BuildState (i+1) s eqs
  return i

-- | Assert that two nodes are equal
addEquality :: Int -> Int -> BuildState e -> BuildState e
addEquality i j (BuildState v e eqs) = BuildState v e $ (i, j) : eqs

addEdge :: Hyperedge e -> BuildState e -> BuildState e
addEdge e (BuildState n es eqs) = BuildState n (e:es) eqs

--------- Build a graph from a term ------------

-- | Build a 'Hypergraph v e' from an 'Expr e'
-- NOTE: This is really dumb - it recomputes types loads of times!
-- TODO: annotate Expr with types instead.
build
  :: (MonadPlus m, MonadState (BuildState e) m)
  => (e -> m (Int, Int)) -- ^ Get type of node
  -> ([Int], [Int])      -- ^ Context nodes - where this term will connect to.
  -> Expr e              -- ^ Expression to graphify
  -> m ()                -- ^ Result contained in state.

-- | Identity means that nodes (ls, rs) are actually the same node.
-- We make a note of this, and renumber nodes later.
build f (ls, rs) Id = do
  case (ls, rs) of
    (l:[], r:[]) -> modify $ addEquality (head ls) (head rs)
    _ -> mzero

-- | Twist is similar
build f (ls, rs) Twist = do
  case (ls, rs) of
    (l1:l2:[], r1:r2:[]) -> do
      modify $ addEquality l1 r2
      modify $ addEquality l2 r1
    _ -> mzero

-- | Build a generator: must have same type as passed-in context (ls, rs)
build f (ls, rs) t@(Generator e) = do
  modify (addEdge $ mkEdge e ls rs)

-- | Sequential composition of terms
build f (ls, rs) t@(Seq x y) = do
  (i, j) <- typeOf f x
  (m, n) <- typeOf f y

  -- Ensure RHS of left term == LHS of right term
  guard (j == m)

  -- Make j == m nodes to connect the two terms
  ns <- replicateM m freshNode

  -- Pass the new nodes to the "built" graph of x and y
  build f (ls, ns) x
  build f (ns, rs) y

-- | Parallel composition of terms: 
build f (ls, rs) t@(Par x y) = do
  (i, j) <- typeOf f x
  (m, n) <- typeOf f y

  build f (take i ls, take j rs) x
  build f (drop i ls, drop j rs) y



range :: Int -> Int -> [Int]
range from to
  | from == to = []
  | otherwise = [from..to-1]

-- Wrapper around 'build'
toGraph'
  :: (MonadPlus m, MonadState (BuildState e) m)
  => (e -> m (Int, Int))
  -> Expr e
  -> m ()
toGraph' f e = do
  (i, j) <- typeOf f e
  put $ BuildState (i+j) [] []
  let ctx = (range 0 i, range i (i+j))
  build f ctx e


toGraph
  :: Show e
  => (e -> Maybe (Int, Int)) -> Expr e -> Maybe (Hypergraph Int e)
toGraph f e =
  case execStateT (toGraph' (g . f) e) s of
    Just s  -> Just (buildStateToGraph s)
    Nothing -> Nothing
  where
    s = BuildState 0 [] [] -- note this is immediately overwritten by toGraph'
    g = maybe mzero return

-- | Turn a BuildState into a Graph
-- TODO: renumber here!
buildStateToGraph :: Show e => BuildState e -> Hypergraph Int e
buildStateToGraph bs = mkGraph vs (reverse es)
  where
    (BuildState n es eqs) = renumber bs
    vs = [0..n-1]

-- | Map a BuildState with a set of node equalities into one with no equalities
-- by renumbering all the nodes.
-- 0. For n nodes
-- 1. Generate set of m < n equivalence classes (the strongly connected
--    components)
-- 2. Number each class [0..m]
-- 3. Generate a map (NodeId -> EquivalenceClassNumber)
-- 4. Replace each node in the graph with its equivalence class
renumber :: Show e => BuildState e -> BuildState e
renumber s@(BuildState n es eqs) =
  BuildState (length sccs) (fmap (renumberEdge m) es) []
  where
    -- Data.Graph assumes directional edges, so we put fwd + back edges.
    -- we also ensure every node is equal to itself.
    g (i,j) = [(i,j), (j,i)]
    eqs' = (eqs >>= g) ++ (let ixs = [0..n-1] in zip ixs ixs)

    f pairs = let i = fst (head pairs) in (i, i, fmap snd pairs)
    xs = fmap f . groupBy ( (==) `on` fst ) . sort $ eqs'

    sccs = zipWith (,) [0..] . fmap flattenSCC . stronglyConnComp $ xs

    -- mapping from NodeId -> ConnectedComponent
    m :: Map Int Int
    m = let kvp (i, js) = fmap (,i) js
        in  Map.fromList . concat $ fmap kvp sccs

-- | Renumber a hyperedge
renumberEdge :: Map Int Int -> Hyperedge e -> Hyperedge e
renumberEdge m (Hyperedge v d c) = Hyperedge v (fmap look d) (fmap look c)
  where
    look i = case Map.lookup i m of
      Nothing -> error $ "couldn't find " ++ show i ++ " in map " ++ show m
      Just  r -> r
