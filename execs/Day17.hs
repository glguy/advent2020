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
import           Advent.Coord (Coord(C))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Array.Unboxed as A

-- | N-dimensional coordinates
type C = [Int]

main :: IO ()
main =
  do inp <- getInputArray 17
     let initial = [[x,y] | (C x y, '#') <- A.assocs inp]
         run x = print (Set.size (iterate step (Set.fromList x) !! 6))
         up = map (0:)
     run (up initial)
     run (up (up initial))

neighborhood :: C -> [C]
neighborhood = traverse \i -> [i-1,i,i+1]

step :: Set C -> Set C
step world
  = Set.filter (rule world)
  $ Set.fromList
  $ concatMap neighborhood
  $ Set.toList world

rule :: Set C -> C -> Bool
rule world c = i == 3 || i == 4 && Set.member c world
  where
    i = length (take 5 (filter (`Set.member` world) (neighborhood c)))
