module Types where

import Data.Vector (Vector(..))
import qualified Data.Vector as Vector

import Control.Monad.Logic
import Control.Monad.State

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


-- TODO: smart constructor; check vertexes are a subset of those referenced in
-- edges.
mkGraph :: [v] -> [Hyperedge e] -> Hypergraph v e
mkGraph vs es = Hypergraph (Vector.fromList vs) (Vector.fromList es)

mkEdge :: a -> [Int] -> [Int] -> Hyperedge a
mkEdge val dom cod = Hyperedge val (Vector.fromList dom) (Vector.fromList cod)

numNodes :: Hypergraph v e -> Int
numNodes = Vector.length . nodes

nodeNames :: Hypergraph v e -> [Int]
nodeNames g
  | numNodes g == 0 = []
  | otherwise = [0 .. numNodes g - 1]
