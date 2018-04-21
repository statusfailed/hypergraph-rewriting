{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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

import Data.String

import Miso (View(..))
import qualified Miso.Html as Html
import Miso.Svg as Svg

import Control.Lens

import Data.Text (Text(..))
import qualified Data.Text as Text

import Data.List (sortBy, groupBy)
import Data.Function (on)

mshow :: Int -> Text
mshow = Text.pack . show

initialNodes :: Hypergraph v e -> [VE]
initialNodes g = fmap V . filter f . nodeNames $ g
  where f x = not $ Vector.any (x `inCodomain`) (edges g)

initialEdges :: Hypergraph v e -> [VE]
initialEdges =
  fmap (E . fst) . filter (Vector.null . dom . snd) . zip [0..] . toList . edges

-- All descendents of ve tagged with their max distance
-- TODO FIXME: doesn't terminate in cyclic graphs!
distFrom :: (Int -> Int -> Int) -> Hypergraph v e -> [VE] -> Map VE Int
distFrom f g ves = Map.unionsWith f (x : fmap (fmap (+1)) xs)
  where
    x  = Map.fromList (fmap (,0) ves)
    xs = fmap (distFrom f g . pure) (neighboursVE g ves)

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
place g = Map.fromList . concat $ zipWith f [1..] (slices g)
  where
    f x col =
      let cumulative = scanl1Of (traverse . _2) (+) . fmap (,1) $ col
      in over (traverse . _2) (Coords x) cumulative


place2 :: Hypergraph v e -> Map VE Coords
place2 g = Map.fromList . concat $ zipWith f [0..] xs
  where
    xs = slices g
    maxDepth = 50 * maximum (fmap length xs) -- TODO
    f x col = zip col . fmap (Coords x) $ portPos 0 maxDepth (length col)

-- TODO: get rid of gross duplication here :-D
drawWiresOfDom :: Hypergraph v e -> Map VE Coords -> Int -> [View action]
drawWiresOfDom g m ei = zipWith f (portPos 5 50 numPorts) ndis
  where
    edge = (edges g ! ei)
    numPorts = Vector.length (dom edge)
    (Coords ex' ey') = m Map.! (E ei)
    ndis = fmap ((m Map.!) . V) $ toList (dom edge)
    f ydelta (Coords nx' ny') =
      let ex = ex'*50
          ey = ey'*50 + ydelta
          nx = nx'*50 + 25
          ny = ny'*50 + 25
      in bezierConnector (nx, ny) (ex, ey)

drawWiresOfCod :: Hypergraph v e -> Map VE Coords -> Int -> [View action]
drawWiresOfCod g m ei = zipWith f (portPos 5 50 numPorts) ndis
  where
    edge = (edges g ! ei)
    numPorts = Vector.length (cod edge)
    (Coords ex' ey') = m Map.! (E ei)
    ndis = fmap ((m Map.!) . V) $ toList (cod edge)
    f ydelta (Coords nx' ny') =
      let ex = ex'*50
          ey = ey'*50 + ydelta
          nx = nx'*50 + 25
          ny = ny'*50 + 25
      in bezierConnector (nx, ny) (ex, ey)

drawWiresOf g m ei = drawWiresOfDom g m ei ++ drawWiresOfCod g m ei

wireStraight :: (Int, Int) -> (Int, Int) -> View action
wireStraight (x1, y1) (x2, y2) = line_
  [ x1_ (mshow x1), y1_ (mshow y1)
  , x2_ (mshow x2), y2_ (mshow y2)
  , stroke_ "red", strokeWidth_ "2"
  ]
  []

bezierConnector :: (Int, Int) -> (Int, Int) -> View action
bezierConnector (x, y) (a, b) =
  path_
    [ d_ (Text.pack svgStr), fill_ "transparent", stroke_ "red"
    , strokeWidth_ "2" 
    ] []
  where
    midX = (a - x) `div` 2
    control1 = (x + 2*midX, y)
    control2 = (a - midX, b)
    showPair (x,y) = show x ++ " " ++ show y
    svgStr = "M " ++ showPair (x,y) ++ " C " ++
             showPair control1 ++ " " ++ showPair control2 ++ " " ++ showPair (a,b)

drawWires :: Hypergraph v e -> Map VE Coords -> [View action]
drawWires g m = edgeNames g >>= drawWiresOf g m

drawVE :: Show e => Hypergraph v e -> VE -> Coords -> View action
drawVE g (V i) (Coords x y) =
  circle_
    [ cx_ (mshow $ x*50 + 25), cy_ (mshow $ y*50 + 25)
    , r_ "3", fill_ "black"] []
drawVE g (E i) (Coords x y) =
  g_ []
    [ rect_
        [ x_ x', y_ y' 
        , width_ "50", height_ "40"
        , fill_ "white", stroke_ "black", rx_ "5", ry_ "5"
        ] []
    , text_ [x_ (mshow (x*50 + 5)), y_ (mshow (y*50 + 30))]
        [ fromString . show . val . (! i) . edges $ g ]
    ]
  where
    x' = mshow $ x*50
    y' = mshow $ y*50 + 5
    yt = mshow $ y*50

edgesAndNodes :: Show e => Hypergraph v e -> Map VE Coords -> [View action]
edgesAndNodes g m = fmap (uncurry $ drawVE g) (Map.toList m)

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
toView f g = svg_ [width_ w, height_ h] $ drawWires g m ++ edgesAndNodes g m
  where
    m = place g
    w = mshow $ 100 * length (slices g)
    h = mshow $ 100 * (maximum . fmap length $ slices g)

go :: IO ()
go = writeFile "/home/sf/foo.html" . show $ page
  where
    page = Html.nodeHtml "html" [s] [Html.body_ [] [toView ebType ebGraph]]
    s = Html.style_ $ Map.fromList [("font-family", "sans")]

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
