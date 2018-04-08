{-# LANGUAGE OverloadedStrings #-}
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

import TestHypergraph (testHypergraphFunctions)
import TestMatching (testMatching)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testHypergraphFunctions
  , testMatching
  ]
