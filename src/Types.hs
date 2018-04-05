module Types where

-- Directed hyperedges
data Hyperedge a = Hyperedge
  { dom :: [a]
  , cod :: [a]
  } deriving(Eq, Ord, Read, Show)

-- Functor on edge type
instance Functor Hyperedge where
  fmap f (Hyperedge d c) = Hyperedge (fmap f d) (fmap f c)


data Hypergraph v e = Hypergraph
  { nodes :: [v]
  , edges :: [Hyperedge e]
  } deriving(Eq, Ord, Read, Show)

-- Functor on edge type
instance Functor (Hypergraph v) where
  fmap f (Hypergraph n e) = Hypergraph n (fmap (fmap f) e)


-- TODO: smart constructor; check vertexes are a subset of those referenced in
-- edges.
mkGraph :: [v] -> [Hyperedge e] -> Hypergraph v e
mkGraph vs es = Hypergraph vs es
