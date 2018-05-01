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

-- | Apply a rewrite rule to a hypergraph.
-- NOTE: assumes both LHS and RHS of the rule have boundary nodes labelled
-- clockwise contiguously 0..n - we use this to relate nodes in the LHS and RHS
-- graphs.
rewrite :: (Show v, Show e, Eq e) => Rule v e -> Hypergraph v e -> Maybe (Hypergraph v e)
rewrite (lhs, rhs) g = do
  -- check rule is valid
  let boundaryL = boundary lhs
  guard $ boundaryL == boundary rhs

  -- get at least one match
  m@(Matching mns mes) <- case match g lhs of [] -> Nothing; (x:xs) -> Just x;

  let n = Vector.length (nodes g)
      isBoundary x = Set.member x boundaryL -- is a node in the LHS boundary matching?
      -- map from RHS Node in (disjoint g rhs) -> Node in g to merge with
      boundaryMap = Bimap.map (+n) . Bimap.filter (const . isBoundary) $ mns
      h = mergeNodes (Map.fromList . Bimap.toList $ boundaryMap) (disjoint g rhs)

  -- H, with L removed
  let lhsNonBoundary ve =
        inMatching m ve && -- VEs in matching
        runVE (not . flip Bimap.memberR boundaryMap) (const True) ve
  return $ cutWhere lhsNonBoundary h


-- | Check if a node or edge is in a Matching
inMatching :: Matching -> VE -> Bool
inMatching (Matching nodes edges) ve = case ve of
  V i -> Bimap.memberR i nodes
  E i -> Bimap.memberR i edges

-- | Fold cases of a VE
-- >>> runVE f g . V = f
-- >>> runVE f g . E = g
runVE :: (Int -> a) -> (Int -> a) -> VE -> a
runVE f _ (V i) = f i
runVE _ g (E i) = g i
