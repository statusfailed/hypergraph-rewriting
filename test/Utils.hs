module Utils where

import Test.Tasty
import Test.Tasty.HUnit
import SMC

-- | Utility to make a predicat about returned matches.
testMatches
  :: (Ord e, Ord v)
  => String -> ([Matching] -> Bool) -> Hypergraph v e -> Hypergraph v e -> TestTree
testMatches msg f graph pattern =
  testCase msg . assertBool msg . f $ match graph pattern
