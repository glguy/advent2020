{-# Language BlockArguments #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/17>

-}
module Main (main) where

import           Advent
import           Data.Set (Set)
import qualified Data.Set as Set

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
  = Set.filter (rule world)
  $ Set.fromList
  $ concatMap neighborhood
  $ Set.toList world

rule :: Set C -> C -> Bool
rule world c = i == 3 || i == 2 && Set.member c world
  where
    i = length (take 4 (filter (`Set.member` world) (neighborhood c)))
