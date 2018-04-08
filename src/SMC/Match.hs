{-# LANGUAGE FlexibleContexts #-}
module SMC.Match where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Logic

import Data.Foldable

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Vector (Vector(..), (!), (!?))
import qualified Data.Vector as Vector

import Data.Maybe (catMaybes)

import Data.Bimap (Bimap(..))
import qualified Data.Bimap as Bimap

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

data MatchTask = V Int | E Int
  deriving(Eq, Ord, Read, Show)

-- | 'match graph pattern' finds an instance of 'pattern' within 'graph', returned as
-- a bidirectional mapping of node and edge IDs.
match
  :: (Eq e, Ord e, Eq v, Ord v)
  => Hypergraph v e
  -> Hypergraph v e
  -> LogicState [MatchTask] Matching
match g p = step emptyMatching where

  -- Run until queue exhausted
  step m = do
    ts <- get
    case ts of
      []      -> return m -- TODO: checks? complete matching?
      (t:ts') -> updateMatching g p t m >>= (\m' -> put ts' >> step m')

-- Update a matching from a MatchTask and current 'Matching'
updateMatching g p t m@(Matching matchedNodes matchedEdges) = do
  case t of
    V pn -> do
      -- propose unmatched nodes
      gn <- bsum $ filter (not . flip Bimap.memberR matchedNodes)
                          [0..Vector.length (nodes g) - 1]
      guard (matchNode g p m pn gn)
      return $ Matching (Bimap.insert pn gn matchedNodes) matchedEdges

    E pe -> do
      ge <- bsum $ filter (not . flip Bimap.memberR matchedEdges)
                          [0..Vector.length (edges g) - 1]
      guard (matchEdge g p m pe ge)
      return $ Matching matchedNodes (Bimap.insert pe ge matchedEdges) 


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

main = runLogicState (match testGraph testPattern) tasks
  where tasks = map V [0..4] ++ map E [0..1]
