{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module SMC.Render where

import SMC.Hypergraph
import SMC.Expr
import Data.Foldable
import Data.Monoid

import Data.Vector (Vector(..), (!))
import qualified Data.Vector as Vector

import Data.Set (Set)
import qualified Data.Set    as Set

import Data.Map (Map(..))
import qualified Data.Map    as Map

import Data.String

import Miso (View(..))
import qualified Miso.Html as Html
import Miso.Svg as Svg
import qualified Miso.String as Miso

import Control.Lens

import Data.List (sortBy, groupBy)
import Data.Function (on)

mshow :: Show a => a -> Miso.MisoString
mshow = Miso.ms . show

drawPad = 5
drawSize = 50
halfDrawSize = drawSize `div` 2

initialNodes :: Hypergraph v e -> [VE]
initialNodes g = fmap V . filter f . nodeNames $ g
  where f x = not $ Vector.any (x `inCodomain`) (edges g)

initialEdges :: Hypergraph v e -> [VE]
initialEdges =
  fmap (E . fst) . filter (Vector.null . dom . snd) . zip [0..] . toList . edges

-- A depth first search from initial nodes, accumulating distance using some function
-- (e.g., min or max).
-- Cycles are not counted.
distFrom' :: (Int -> Int -> Int) -> Set VE -> Hypergraph v e -> [VE] -> Map VE Int
distFrom' f visited g ves = Map.unionsWith f (x : fmap (fmap (+1)) xs)
  where
    -- current set of distances
    x  = Map.fromList (fmap (,0) ves)
    -- depth-first recursed distances
    xs = fmap go $ filter (flip Set.notMember visited) (neighboursVE g ves)
    go x = distFrom' f (Set.union visited $ Set.singleton x) g [x]

-- All descendents of ve tagged with their max distance.
-- TODO FIXME: doesn't always terminate in cyclic graphs
distFrom :: (Int -> Int -> Int) -> Hypergraph v e -> [VE] -> Map VE Int
distFrom f g ves = distFrom' f Set.empty g ves

-- add nodes missing from a distance map, but appearing in a graph.
addMissing :: Hypergraph v e -> Map VE Int -> Map VE Int
addMissing g m = Map.union missing m
  where
    maxDist = Map.foldl' max 0 m
    allVEs  = fmap V (nodeNames g) ++ fmap E (edgeNames g)
    missing = Map.fromList . fmap (,maxDist + 1) . filter (flip Map.notMember m) $ allVEs

-- | Traverse the graph from left to right.
--
-- NOTE: this function is broken under Frob, because it will not find any
-- initial nodes or edges (consider the graph with one node connected to a
-- (1,1) edge on both its domain and codomain.
slices :: Hypergraph v e -> [[VE]]
slices g = fmap (fmap fst)
         . groupBy ((==) `on` snd)
         . sortBy (compare `on` snd)
         . Map.toList
         . addMissing g
         . distFrom max g
         $ initialNodes g ++ initialEdges g

------------

data Coords = Coords
  { x :: Int
  , y :: Int
  } deriving(Eq, Ord, Read, Show)

instance Monoid Coords where
  mempty = Coords 0 0
  mappend (Coords x1 y1) (Coords x2 y2) = Coords (x1+x2) (y1+y2)

-- | y coordinates of ports on a Hyperedge, given padding and total height.
--
-- >>> portPos 20 100 2 == [40, 60]
portPos :: Int -> Int -> Int -> [Int]
portPos pad height numPorts = fmap (\i -> pad + i*delta) [1..numPorts]
  where
    boxHeight = height - (2*pad)
    delta = boxHeight `div` (numPorts + 1)

place :: Hypergraph v e -> Map VE Coords
place g = Map.fromList . concat $ zipWith f [0..] (slices g)
  where
    f x col = zipWith (,) col . fmap (Coords $ drawSize*x) $ fmap (*drawSize) [0..]

place2 :: Hypergraph v e -> Map VE Coords
place2 g = Map.fromList . concat $ zipWith f [0..] cols
  where
    cols = slices g
    maxDepth = drawSize * maximum (fmap length cols)
    f x col =
      let depth = drawSize * length col
      in zip col . fmap (Coords $ drawSize*x) $ portPos 0 depth (length col)

-- TODO: get rid of gross duplication here :-D
drawWiresOfDom :: Hypergraph v e -> Map VE Coords -> Int -> [View action]
drawWiresOfDom g m ei = zipWith f (portPos drawPad drawSize numPorts) ndis
  where
    edge = (edges g ! ei)
    numPorts = Vector.length (dom edge)
    (Coords ex ey) = m Map.! (E ei)
    ndis = fmap ((m Map.!) . V) $ toList (dom edge)
    f ydelta (Coords nx ny) =
      bezierConnector (nx+halfDrawSize, ny+halfDrawSize) (ex, ey + ydelta)

drawWiresOfCod :: Hypergraph v e -> Map VE Coords -> Int -> [View action]
drawWiresOfCod g m ei = zipWith f (portPos drawPad drawSize numPorts) ndis
  where
    edge = (edges g ! ei)
    numPorts = Vector.length (cod edge)
    (Coords ex ey) = m Map.! (E ei)
    ndis = fmap ((m Map.!) . V) $ toList (cod edge)
    f ydelta (Coords nx ny) =
      bezierConnector (nx+halfDrawSize, ny+halfDrawSize) (ex+drawSize, ey + ydelta)

drawWiresOf g m ei = drawWiresOfDom g m ei ++ drawWiresOfCod g m ei

wireStraight :: (Int, Int) -> (Int, Int) -> View action
wireStraight (x1, y1) (x2, y2) = line_
  [ x1_ (mshow x1), y1_ (mshow y1)
  , x2_ (mshow x2), y2_ (mshow y2)
  , stroke_ "black", strokeWidth_ "2"
  ]
  []

bezierConnector :: (Int, Int) -> (Int, Int) -> View action
bezierConnector (x, y) (a, b) =
  path_
    [ d_ (Miso.ms svgStr), fill_ "transparent", stroke_ "#2B4FFC"
    , strokeWidth_ "2" 
    ] []
  where
    midX = (a - x) `div` 2
    control1 = (x + midX, y)
    control2 = (a - midX, b)
    showPair (x,y) = show x ++ " " ++ show y
    svgStr = "M " ++ showPair (x,y) ++ " C " ++
             showPair control1 ++ " " ++ showPair control2 ++ " " ++ showPair (a,b)

drawWires :: Hypergraph v e -> Map VE Coords -> [View action]
drawWires g m = edgeNames g >>= drawWiresOf g m

drawVE
  :: Map VE String  -- ^ Stroke colours
  -> (e -> String)  -- ^ How to display an edge
  -> Hypergraph v e -- ^ graph we're drawing
  -> VE             -- ^ Current item to draw
  -> Coords         -- ^ Position of item
  -> View action
drawVE strokes showE g (V i) (Coords x y) =
  g_ []
    [ circle_
        [ cx_ (mshow $ x + halfDrawSize), cy_ (mshow $ y + halfDrawSize)
        , r_ "3", fill_ (Miso.ms col)] []
    {-, text_ [x_ (mshow $ x + halfDrawSize), y_ (mshow $ y + halfDrawSize)]-}
        {-[fromString $ show i]-}
    ]
  where
    col = maybe "black" id (Map.lookup (V i) strokes)
drawVE strokes showE g (E i) (Coords x y) =
  g_ []
    [ rect_
        [ x_ x', y_ y' 
        , width_ (mshow drawSize), height_ (mshow $ drawSize - 2*drawPad)
        , fill_ "white", stroke_ (Miso.ms col), rx_ "5", ry_ "5"
        ] []
    , text_ [x_ (mshow (x + 5)), y_ (mshow (y + (drawSize `div` 2) + drawPad))]
        [ fromString . showE . val . (! i) . edges $ g ]
    ]
  where
    x' = mshow $ x
    y' = mshow $ y + 5
    yt = mshow $ y
    col = maybe "black" id (Map.lookup (E i) strokes)

edgesAndNodes
  :: Map VE String
  -> (e -> String)
  -> Hypergraph v e
  -> Map VE Coords
  -> [View action]
edgesAndNodes stroke showE g m =
  fmap (uncurry $ drawVE stroke showE g) (Map.toList m)

-- | Render a hypergraph as a Miso 'View'.
--
-- 1. Produce a list of "slices" in the graph: [nodes, edges, nodes, ...] :: [[VE]]
-- 2. Fix width of all elements; precompute width of each column.
-- 3. compute height of each element, annotate. [[(VE, Int)]]
-- 4. fold down heights of each column to compute y position [[(VE, Int)]]
-- 5. Flatten list, draw each element
--
-- Edges?
toView
  :: Map VE String     -- ^ Stroke colours of each item
  -> (e -> String)     -- ^ How to display an edge
  -> (e -> (Int, Int)) -- ^ Type of an edge
  -> Hypergraph v e
  -> (View action)
toView strokes showE f g =
  svg_ [width_ w, height_ h] $ edgesAndNodes strokes showE g m ++ drawWires g m
  where
    m = place g
    w = mshow $ drawSize * length (slices g)
    h = mshow $ drawSize * (maximum . fmap length $ slices g)
