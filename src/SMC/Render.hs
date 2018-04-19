module SMC.Render where

import SMC.Hypergraph
import SMC.Expr
import Data.Foldable
import Data.Monoid

import Data.Vector (Vector(..), (!))
import qualified Data.Vector as Vector

import qualified Data.Set    as Set

import Data.Map (Map(..))
import qualified Data.Map    as Map

import Miso (View(..))
import Miso.Svg as Svg

initialNodes :: Hypergraph v e -> [VE]
initialNodes g = fmap V . filter f . nodeNames $ g
  where f x = not $ Vector.any (x `inCodomain`) (edges g)

initialEdges :: Hypergraph v e -> [VE]
initialEdges =
  fmap (E . fst) . filter (Vector.null . dom . snd) . zip [0..] . toList . edges

-- | Traverse the graph from left to right.
--
-- NOTE: this function is broken under Frob, because it will not find any
-- initial nodes or edges (consider the graph with one node connected to a
-- (1,1) edge on both its domain and codomain.
slices :: Hypergraph v e -> [[VE]]
slices g = go Set.empty start
  where
    start = initialNodes g ++ initialEdges g

    go visited current
      | null current = []
      | otherwise =
        let visited' = Set.union visited (Set.fromList current)
            current' = filter (flip Set.notMember visited) (neighboursVE g current)
        in  current : go visited' current'

------------

data DrawInfo = DrawInfo
  { x :: Int
  , y :: Int
  } deriving(Eq, Ord, Read, Show)

instance Monoid DrawInfo where
  mempty = DrawInfo 0 0
  mappend (DrawInfo x1 y1) (DrawInfo x2 y2) = DrawInfo (x1+x2) (y1+y2)

-- Height in "number of wires"
wireHeight :: Hypergraph v e -> (e -> (Int, Int)) -> VE -> Int
wireHeight _ _ (V v) = 1
wireHeight g f (E e) = uncurry max . f . val $ edges g ! e

-- Given a "padding", compute actual height of an element from the number of wires.
actualHeight :: Int -> Int -> Int
actualHeight p w = p + p*(w+1) + p

-- | y coordinates of ports for a given
{-portPos :: Int -> Int -> Int -> Vector Int-}
{-portPos padding boxHeight numPorts = Vector.fromList ys-}
  {-where ys = fmap (+padding) . scanl (+) 0 $ [1..numPorts]-}

place :: (e -> (Int, Int)) -> Hypergraph v e -> Map VE DrawInfo
place edgeType g = Map.fromList . concat $ zipWith f [0..] (slices g)
  where
    f x col =
      let unplaced   = fmap (\ve -> (ve, wireHeight g edgeType ve)) col
          cumulative = scanl1 (\acc (ve, y) -> (ve, y + snd acc)) unplaced
      in fmap (\(v,y) -> (v, DrawInfo x y)) cumulative

wires :: (e -> (Int, Int)) -> Hypergraph v e -> Map VE DrawInfo -> [View action]
wires etype g info = undefined

edgesAndNodes :: (e -> (Int, Int)) -> Map VE DrawInfo -> [View action]
edgesAndNodes g info = undefined

-- | Render a hypergraph as a Miso 'View'.
--
-- 1. Produce a list of "slices" in the graph: [nodes, edges, nodes, ...] :: [[VE]]
-- 2. Fix width of all elements; precompute width of each column.
-- 3. compute height of each element, annotate. [[(VE, Int)]]
-- 4. fold down heights of each column to compute y position [[(VE, Int)]]
-- 5. Flatten list, draw each element
--
-- Edges?
toView :: (Show v, Show e) => (e -> (Int, Int)) -> Hypergraph v e -> View action
toView f g = undefined

------------- Debugging -----------

data EB = EB1 | EB2 | EB3 | EB4
  deriving (Eq, Ord, Read, Show)

ebType :: EB -> (Int, Int)
ebType x = case x of
  EB1 -> (1, 2)
  EB2 -> (2, 1)
  EB3 -> (1, 1)
  EB4 -> (1, 1)

ebGraph :: Hypergraph Int EB
Just ebGraph = toGraph (return . ebType) expr
  where
    expr = foldl1 Seq [Generator EB1, Par (Generator EB3) Id, Generator EB2]
