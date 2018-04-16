{-# LANGUAGE OverloadedStrings #-}
module TestPaper where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Control.Monad.Logic

import Data.List (sort, inits, tails)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import qualified Data.Bimap as Bimap

import Data.Foldable

import SMC
import Utils (testMatches)

-- This module tests examples 4.8 and 5.3 from the paper:
-- http://users.ecs.soton.ac.uk/ps/papers/rewriting.pdf

testPaper = testGroup "examples from paper" unitTests

unitTests =
  -- Test that matching the LHS of example 4.8 in its context yields exactly one match;
  -- this tests matching *without* the convexity and boundary complement restrictions
  [ testMatches "example 4.8" ((==1) . length) eaGraph eaLHS

  -- Same as above, but for example 5.3. Ensure exactly one match for LHS,
  -- assuming no convexity condition.
  , testMatches "example 5.3" ((==1) . length) ebGraph ebLHS

  -- Test for example 5.3, but with ensure convexity condition is true
  , let f g (Matching ns es) = convex g . fmap snd . Bimap.toList $ ns
        h = filter (f ebGraph)
    in  testMatches "example 5.3 (convex)" ((==0) . length . h) ebGraph ebLHS
  ]

------------ Example 4.8 -------------

-- | Signature for Example 4.8
-- the "A" in EA is just to distinguish from Example 5.3 later. Kinda hacky I
-- know!
data EA = EA1 | EA2 | EA3
  deriving(Eq, Ord, Read, Show)

-- | Map an element of sigma for example 48 to its type
eaType :: EA -> (Int, Int)
eaType x = case x of
  EA1    -> (0, 1)
  EA2    -> (1, 0)
  EA3    -> (1, 1)

-- | Build the graph
eaGraph :: Hypergraph Int EA
Just eaGraph = toGraph (return . eaType) expr
  where expr = Seq (Generator EA1) (Generator EA2)

-- | Left-hand-side of the rule from example 4.8
eaLHS :: Hypergraph Int EA
(Just eaLHS) = toGraph (return . eaType) Id


------------ Example 5.3 -------------

data EB = EB1 | EB2 | EB3 | EB4
  deriving(Eq, Ord, Read, Show)

ebType :: EB -> (Int, Int)
ebType x = case x of
  EB1 -> (1, 2)
  EB2 -> (2, 1)
  EB3 -> (1, 1)
  EB4 -> (1, 1)

-- Left-hand-side of the rewrite rule from example 5.3
ebLHS :: Hypergraph Int EB
Just ebLHS = toGraph (return . ebType) expr
  where
    expr = foldl1 Seq
      [ Par Id (Generator EB1)
      , Par Twist Id
      , Par Id (Generator EB2)
      ]

-- Graph (context) from example 5.3
ebGraph :: Hypergraph Int EB
Just ebGraph = toGraph (return . ebType) expr
  where
    expr = foldl1 Seq [Generator EB1, Par (Generator EB3) Id, Generator EB2]
