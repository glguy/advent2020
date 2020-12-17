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

neighborhood :: C -> [C]
neighborhood = tail . traverse \i -> [i,i-1,i+1]

step :: Set C -> Set C
step world
  = Map.keysSet
  $ Map.filterWithKey (rule world)
  $ Map.fromListWith (+) [(n,1) | s <- Set.toList world, n <- neighborhood s]

rule :: Set C -> C -> Int -> Bool
rule world c i = i == 3 || i == 2 && Set.member c world
