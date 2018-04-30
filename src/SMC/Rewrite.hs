module SMC.Rewrite where

import Control.Monad

import SMC.Hypergraph
import SMC.Match

import Data.Foldable

import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Map as Map

import Control.Lens

type Rule v e = (Hypergraph v e, Hypergraph v e)

-- Given a rewrite rule
rewrite :: (Show v, Show e, Eq e) => Rule v e -> Hypergraph v e -> Maybe (Hypergraph v e)
rewrite (lhs, rhs) g = do
  -- check rule is valid
  guard $ boundary lhs == boundary rhs

  -- get at least one match
  m@(Matching mns mes) <- case match g lhs of [] -> Nothing; (x:xs) -> Just x;

  -- NOTE: we are using a fact that all hypergraphs have their boundary nodes
  -- anticlockwise contiguously labelled 0..n, we can rely on both sides of a
  -- rule to have the same node IDs.
  let boundaryL = Set.map (V . (mns Bimap.!)) (boundary lhs)

  -- tack on rhs, and merge its nodes into g.
  --  1. tacking - nodes in RHS all increase by n
  --  2. rewrite - merge all boundary RHS nodes with matching of *boundary* nodes from LHS
  --  3. Cut out all non-boundary LHS nodes
  let n = Vector.length (nodes g) - Vector.length (nodes lhs) + Set.size boundaryL
      nonBoundary = filter (flip Set.member boundaryL . V . snd) $ Bimap.toList mns
      rewriteMap = Map.fromList $ over (traverse . _1) (n+) $ nonBoundary
      h = mergeNodes rewriteMap (disjoint g rhs)

  -- H, with L cut out.
  return $ cutWhere (\ve -> inMatching m ve && Set.notMember ve boundaryL) h


-- | Check if a node or edge is in a Matching
inMatching :: Matching -> VE -> Bool
inMatching (Matching nodes edges) ve = case ve of
  V i -> Bimap.memberR i nodes
  E i -> Bimap.memberR i edges
