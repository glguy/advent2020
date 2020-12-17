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
import Advent.Bench
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
     let run x = print (Set.size (iterate step (Set.fromList x) !! 6))
         up = map (0:)
     run (up inp)
     run (up (up inp))

-- | Compute distance 1 neighborhood around a coordinate.
--
-- >>> neighborhood [10,20]
-- [[10,19],[10,21],[9,20],[9,19],[9,21],[11,20],[11,19],[11,21]]
neighborhood :: C -> [C]
neighborhood = tail . traverse \i -> [i,i-1,i+1]

-- | Compute the next generation from the previous generation
step :: Set C -> Set C
step world
  = Map.keysSet
  $ Map.filterWithKey (rule world)
  $ Map.fromListWith (+) [(n,1) | s <- Set.toList world, n <- neighborhood s]

-- | Determine if a cell should be alive in the next generation.
rule ::
  Set C {- ^ previous generation      -} ->
  C     {- ^ coordinate               -} ->
  Int   {- ^ live neighbor count      -} ->
  Bool  {- ^ alive in next generation -}
rule world c n = n == 3 || n == 2 && Set.member c world
