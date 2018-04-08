{-# LANGUAGE OverloadedStrings #-}
module TestMatching where

import Test.Tasty
{-import Test.Tasty.SmallCheck as SC-}
{-import Test.Tasty.QuickCheck as QC-}
import Test.Tasty.HUnit

import Control.Monad
import Control.Monad.Logic

import Data.List (sort, inits, tails)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Foldable

import SMC
import Utils (testMatches)

testMatching = testGroup "test matching" unitTests

unitTests :: [TestTree]
unitTests =
  [ testMatches "testGraph has matches" (not . null) testGraph testPattern
  ]

----- Unit test example data -----

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
