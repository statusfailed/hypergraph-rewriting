{-# LANGUAGE FlexibleContexts #-}
module SMC.Match where

import Control.Lens hiding ((|>), (<|), (:<), (>:))
import Control.Monad
import Control.Monad.State
import Control.Monad.Logic

import Data.Foldable

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Set (Set(..), (\\))
import qualified Data.Set as Set

import Data.Vector (Vector(..), (!), (!?))
import qualified Data.Vector as Vector

import Data.Sequence (Seq(..), (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq

import Data.Maybe (catMaybes)

import Data.Bimap (Bimap(..))
import qualified Data.Bimap as Bimap

import Data.List (nub)

import SMC.Hypergraph
import SMC.Util (bsum)

data Matching = Matching
  { matchedNodes :: Bimap Int Int
  , matchedEdges :: Bimap Int Int
  } deriving(Eq, Ord, Show)

emptyMatching = Matching Bimap.empty Bimap.empty

edgeIndices :: (Hyperedge a -> Vector Int) -> Int -> Hyperedge a -> Vector Int
edgeIndices f i = Vector.findIndices (==i) . f

-- Match conditions 2:

-- Two invariants:
-- A match on nodes pn and gn means:
--
--  -1. Label must match
--  0. pn can't be already matched
--  2. gn can't already be matched (mapping pn -> gn must not have pn' -> gn)
--  1. Matched edges containing pn must do so in the same place (pattern <-> graph)
matchNode :: Hypergraph v e -> Hypergraph v e -> Matching -> Int -> Int -> Bool
matchNode g p s@(Matching matchedNodes matchedEdges) patNode graphNode
  = unmatched &&
    (Vector.all id $ Vector.imap (checkEdge g p s patNode graphNode) (edges p))

  where
    unmatched :: Bool
    unmatched =
      not (Bimap.member  patNode   matchedNodes) &&
      not (Bimap.memberR graphNode matchedNodes)

-- Check for contradictions with edges connected to propose nodes
checkEdge
  :: Hypergraph v e -> Hypergraph v e -> Matching -> Int -> Int
  -> Int -> Hyperedge e -> Bool
checkEdge
  g p (Matching matchedNodes matchedEdges)
  patNodeIx graphNodeIx
  patternIndex patEdge =
  case Bimap.lookup patternIndex matchedEdges of
    -- If pattern edge index doesn't match with any edges, no contradiction.
    Nothing -> True

    -- If it does match, make sure patNode and graphNode appear in same positions
    -- in its domain and codomain.
    Just graphIndex ->
      let graphEdge  = edges g ! graphIndex
          domIndices =
            edgeIndices dom patNodeIx patEdge == edgeIndices dom graphNodeIx graphEdge
          codIndices =
            edgeIndices cod patNodeIx patEdge == edgeIndices cod graphNodeIx graphEdge
      in domIndices && codIndices

-- Matching edges
--  1. Label must match
--  2. Type must match
--  3. For pairs of nodes in pat cod, graph cod:
--      3.1: Must both be either unmatched, or mapped together
matchEdge :: Eq e => Hypergraph v e -> Hypergraph v e -> Matching -> Int -> Int -> Bool
matchEdge g p s@(Matching matchedNodes matchedEdges) patEdgeIx graphEdgeIx =
  -- Make sure patEdgeIx and graphEdgeIx are in the graph!
  case liftM2 (,) patEdgeM graphEdgeM of
    Just (patEdge, graphEdge) ->
         (val patEdge == val graphEdge)
      && edgeEquiv matchedNodes (dom patEdge) (dom graphEdge)
      && edgeEquiv matchedNodes (cod patEdge) (cod graphEdge)
    Nothing -> False
  where
    patEdgeM   = edges p !? patEdgeIx
    graphEdgeM = edges g !? graphEdgeIx

-- Check that two vectors are equivalent to each other under some bidirectional
-- mapping
edgeEquiv :: (Eq a, Ord a) => Bimap a a -> Vector a -> Vector a -> Bool
edgeEquiv m xs ys
  =  Vector.length xs == Vector.length ys
  && Vector.all id (Vector.izipWith f xs ys)
  where
    -- Either x and y are unmatched, or they match to each other.
    f i x y =
      let xy = Bimap.lookup  x m
          yx = Bimap.lookupR y m
      in (xy == Nothing && yx == Nothing) || (xy == Just y && yx == Just x)


------------

-- Update a 'Matching' from a VE and current 'Matching'
updateMatching
  :: (Eq e, MonadLogic m)
  => Hypergraph v e -> Hypergraph v e -> VE -> Matching -> m Matching
updateMatching g p t m@(Matching matchedNodes matchedEdges) = do
  case t of
    V pn -> do
      -- propose unmatched nodes
      gn <- bsum $ filter (not . flip Bimap.memberR matchedNodes) (nodeNames g)
      guard (matchNode g p m pn gn)
      return $ Matching (Bimap.insert pn gn matchedNodes) matchedEdges

    E pe -> do
      ge <- bsum $ filter (not . flip Bimap.memberR matchedEdges) (edgeNames g)
      guard (matchEdge g p m pe ge)
      return $ Matching matchedNodes (Bimap.insert pe ge matchedEdges)


-- | 'match graph pattern' finds an instance of 'pattern' within 'graph', returned as
-- a bidirectional mapping of node and edge IDs.
match'
  :: (Eq e, Ord e, Eq v, Ord v)
  => Hypergraph v e
  -> Hypergraph v e
  -> LogicState [VE] Matching
match' g p = step emptyMatching where

  -- Run until queue exhausted
  step m = do
    ts <- get
    case ts of
      []      -> return m -- TODO: checks? complete matching?
      (t:ts') -> updateMatching g p t m >>= (\m' -> put ts' >> step m')

match :: (Ord e, Ord v) => Hypergraph v e -> Hypergraph v e -> [Matching]
match g p = fmap fst $ runLogicState (match' g p) $ taskBfs p


------------ Breadth-First traversals -----------

type BfsState = (Set VE, Seq VE, Seq VE)

-- Immediate successors of the current task.
-- NOTE: this never goes "backwards" - only moves along directed edges
taskSucc :: Hypergraph v e -> VE -> Vector VE
taskSucc g (E e) = maybe Vector.empty (fmap V . cod) (edges g !? e)
taskSucc g (V v)
  = fmap (E . fst)
  . Vector.filter snd
  . Vector.imap (\i e -> (i, inDomain v e)) $ (edges g)

taskBfs' :: Hypergraph v e -> State BfsState ()
taskBfs' g = do
  (visited, todo, done) <- get
  case Seq.viewl todo of
    Seq.EmptyL -> return ()
    h :< _     -> do
      forM_ (taskSucc g h) (addTask visited)
      complete h
      modify (over _2 $ Seq.drop 1)
      taskBfs' g
  return ()

  where
    addTask v t = when (Set.notMember t v) $ modify (over _2 (|> t))
    complete t  = modify (over _3 (|> t)) >> modify (over _1 $ Set.insert t)

-- | Breadth-first traversal of a Hypergraph, returning a list of nodes and edges.
-- Starts from the "left dangling wires" of the graph
taskBfs :: Hypergraph v e -> [VE]
taskBfs g
  | Vector.null (nodes g) = []
  | otherwise = nub . toList . view _3 $ execState (taskBfs' g) s
  where
    -- start at the "left dangling wires" of the graph
    s = (Set.empty, start, Seq.empty)
    start = Seq.fromList . fmap V . toList . fst $ freeNodes g
