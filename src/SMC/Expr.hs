{-# LANGUAGE FlexibleContexts #-}
module SMC.Expr where

import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Catch

import SMC.Hypergraph
import SMC.Match

data Expr e
  = Generator e
  | Seq (Expr e) (Expr e)
  | Par (Expr e) (Expr e)
  deriving(Eq, Ord, Read, Show)

-- | Calculate "type" of expression given a function returning types of generators.
typeOf :: (MonadPlus m) => (e -> m (Int, Int)) -> Expr e -> m (Int, Int)
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

freshNode :: MonadState (BuildState e) m => m Int
freshNode = do
  (BuildState i s) <- get
  put $ BuildState (i+1) s
  return i

data BuildState e = BuildState
  { numVars  :: Int
  , newEdges :: [Hyperedge e] -- ^ NOTE: this should really be in a WriterT
  } deriving(Eq, Ord, Read, Show)

-- | Build a 'Hypergraph v e' from an 'Expr e'
-- NOTE: This is really dumb - it recomputes types loads of times!
-- TODO: annotate Expr with types instead.
build
  :: (MonadPlus m, MonadState (BuildState e) m)
  => (e -> m (Int, Int)) -- ^ Get type of node
  -> ([Int], [Int])      -- ^ Context nodes - where this term will connect to.
  -> Expr e              -- ^ Expression to graphify
  -> m ()                -- ^ Result contained in state.
build f (ls, rs) t@(Generator e) = do
  (BuildState n es) <- get
  put $ BuildState n (mkEdge e ls rs : es)

build f (ls, rs) t@(Seq x y) = do
  (i, j) <- typeOf f x
  (m, n) <- typeOf f y
  guard (j == m)

  ns <- replicateM m freshNode

  build f (ls, ns) x
  build f (ns, rs) y

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
  put $ BuildState (i+j) []
  let ctx = (range 0 (i-1), range i (i+j))
  build f ctx e

buildStateToGraph (BuildState n es) = mkGraph vs (reverse es)
  where vs = [0..n-1]

toGraph :: (e -> Maybe (Int, Int)) -> Expr e -> Maybe (Hypergraph Int e)
toGraph f e =
  case execStateT (toGraph' (g . f) e) s of
    Just s  -> Just (buildStateToGraph s)
    Nothing -> Nothing
  where
    s = BuildState 0 [] -- note this is immediately overwritten by toGraph'
    g = maybe mzero return
