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
      e = Seq (Par (Generator Id) (Generator Add)) (Generator Add)
      (Just g) = toGraph (return . iaType) $ double e
      (Just p) = toGraph (return . iaType) e
    in testMatches "test doubling has at least two matches" ((>=2) . length) g p
  ]

double :: Expr e -> Expr e
double t = Par t t

example = toGraph return $ Seq (Generator (1,2)) (Generator (2,1))
example2 = toGraph return $ Par (Generator (1,1)) (Generator (1,1))

-- A simple system with two generators, Id and Add
data IA = Id | Add
  deriving(Eq, Ord, Read, Show)

iaType Id  = (1,1)
iaType Add = (2,1)

