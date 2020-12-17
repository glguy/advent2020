{-# Language ImportQualifiedPost, BlockArguments #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/17>

-}
module Main (main) where

import Advent
import Control.Monad (replicateM)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | N-dimensional coordinates
type C = [Int]

-- | Find the coordinates of live cells.
--
-- >>> parse [".#.", "..#", "###"]
-- [[1,0],[2,1],[0,2],[1,2],[2,2]]
parse :: [String] -> [C]
parse input = [[x,y] | (y,line) <- zip [0..] input, (x,'#') <- zip [0..] line]

-- |
-- >>> :main
-- 257
-- 2532
main :: IO ()
main =
  do inp <- parse <$> getInputLines 17
     print (run 3 inp)
     print (run 4 inp)

run ::
  Int     {- ^ dimension                      -} ->
  [[Int]] {- ^ input x,y coordinates          -} ->
  Int     {- ^ live cells after 6 generations -}
run d
  = Set.size
  . times 6 (step (neighborhood d))
  . Set.mapMonotonic (replicate (d-2) 0 ++)
  . Set.fromList

-- | Compute distance 1 neighborhood around the origin for a given dimension.
--
-- >>> neighborhood 2
-- fromList [([-1,-1],1),([-1,0],1),([-1,1],1),([0,-1],1),([0,1],1),([1,-1],1),([1,0],1),([1,1],1)]
neighborhood :: Int -> Map C Int
neighborhood d = Map.fromList [(c,1) | c <- tail (replicateM d [0,-1,1])]

-- | Compute the next generation from the previous generation
step :: Map C Int -> Set C -> Set C
step d world
  = Map.keysSet
  $ Map.filterWithKey (rule world)
  $ Map.unionsWith (+) [Map.mapKeysMonotonic (zipWith (+) s) d | s <- Set.toList world ]

-- | Determine if a cell should be alive in the next generation.
rule ::
  Set C {- ^ previous generation      -} ->
  C     {- ^ coordinate               -} ->
  Int   {- ^ live neighbor count      -} ->
  Bool  {- ^ alive in next generation -}
rule world c n = n == 3 || n == 2 && Set.member c world
