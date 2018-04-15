{-# LANGUAGE OverloadedStrings #-}
module TestExpr where

import Test.Tasty
{-import Test.Tasty.SmallCheck as SC-}
{-import Test.Tasty.QuickCheck as QC-}
import Test.Tasty.HUnit

import Control.Monad
import Control.Monad.Logic

import Data.List (sort, inits, tails, nub)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Foldable
import qualified Data.Bimap as Bimap

import SMC
import Utils (testMatches)

testExpr = testGroup "test Expr" unitTests

matchedAllNodesAndEdges :: Hypergraph v e -> Matching -> Bool
matchedAllNodesAndEdges (Hypergraph ns es) (Matching mn me) =
  length ns == length (Bimap.toList mn) && length es == length (Bimap.toList me)

unitTests =
  [ testMatches "test doubling has at least two matches" ((>=2) . length)
      exampleGraph examplePattern

  , testMatches "test match is right size" (all $ matchedAllNodesAndEdges examplePattern)
      exampleGraph examplePattern

  , let g = exampleGraph
        f = nub . sort
    in  testCase "taskBfs visits all nodes" . assertBool "" $
        f (taskBfs g) == f (fmap V (nodeNames g) ++ fmap E (edgeNames g))

  , let g = exampleGraph
    in  testCase "taskBfs visits all nodes once" . assertBool "" $
        length (taskBfs g) == length (nodes g) + length (edges g)

  , let
      (Just g) = toGraph return Id
    in testCase "toGraph Id has one node and no edges" . assertBool "" $
       length (nodes g) == 1 && length (edges g) == 0
  ]

-- A simple system with one generator, Add
data IA = Add
  deriving(Eq, Ord, Read, Show)

iaType Add = (2,1)

double :: Expr e -> Expr e
double t = Par t t

exampleExpr = Seq (Par Id (Generator Add)) (Generator Add)
(Just exampleGraph) = toGraph (return . iaType) $ double exampleExpr
(Just examplePattern) = toGraph (return . iaType) exampleExpr
