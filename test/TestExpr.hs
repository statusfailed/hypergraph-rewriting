{-# LANGUAGE OverloadedStrings #-}
module TestExpr where

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

testExpr = testGroup "test Expr" unitTests

unitTests =
  [ let
      e = Seq (Par Id (Generator Add)) (Generator Add)
      (Just g) = toGraph (return . iaType) $ double e
      (Just p) = toGraph (return . iaType) e
    in testMatches "test doubling has at least two matches" ((>=2) . length) g p
  , let
      msg = "toGraph Id has one node and no edges"
      (Just g) = toGraph return Id
    in testCase msg . assertBool msg $ length (nodes g) == 1 && length (edges g) == 0
  ]

double :: Expr e -> Expr e
double t = Par t t

example = toGraph return $ Seq (Generator (1,2)) (Generator (2,1))
example2 = toGraph return $ Par (Generator (1,1)) (Generator (1,1))

-- A simple system with one generator, Add
data IA = Add
  deriving(Eq, Ord, Read, Show)

iaType Add = (2,1)
