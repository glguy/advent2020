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

-- | Find the coordinates of live cells.
--
-- >>> parse [".#.", "..#", "###"]
-- [[1,0],[2,1],[0,2],[1,2],[2,2]]
parse :: [String] -> [(Int,Int)]
parse input = [(x,y) | (y,line) <- zip [0..] input, (x,'#') <- zip [0..] line]

-- |
-- >>> :main
-- 257
-- 2532
main :: IO ()
main =
  do inp <- parse <$> getInputLines 17
     print (run (map toC3 inp))
     print (run (map toC4 inp))

run ::
  Coord a =>
  [a] {- ^ input coordinates              -} ->
  Int {- ^ live cells after 6 generations -}
run = Set.size . times 6 step . Set.fromList

-- | Determine if a cell should be alive in the next generation.
rule ::
  Ord a =>
  Set a {- ^ previous generation      -} ->
  a     {- ^ coordinate               -} ->
  Int   {- ^ live neighbor count      -} ->
  Bool  {- ^ alive in next generation -}
rule world c n = n == 3 || n == 2 && Set.member c world

-- | Compute the next generation from the previous generation
step :: Coord a => Set a -> Set a
step world
  = Map.keysSet
  $ Map.filterWithKey (rule world)
  $ Map.unionsWith (+)
  $ map neighborCount
  $ Set.toList world

-- Unpacked, custom tuples improves my runtime by a factor of 3 over the list of Int version

data C3 = C3 !Int !Int !Int      deriving (Eq, Ord)
data C4 = C4 !Int !Int !Int !Int deriving (Eq, Ord)

toC3 :: (Int,Int) -> C3
toC3 (x,y) = C3 x y 0

toC4 :: (Int,Int) -> C4
toC4 (x,y) = C4 x y 0 0

-- | Compute a Map with @1@ stored at each neighboring coordinate
neighborCount :: Coord a => a -> Map a Int
neighborCount c = Map.mapKeysMonotonic (add c) neighborhood

class Ord a => Coord a where
  add          :: a -> a -> a
  neighborhood :: Map a Int -- Defined as a CAF so that it is only computed once

instance Coord C3 where
  add (C3 a b c) (C3 x y z) = C3 (a+x) (b+y) (c+z)
  neighborhood = Map.fromList [(C3 x y z,1) | [x,y,z] <- tail (replicateM 3 [0,-1,1])]

instance Coord C4 where
  add (C4 a b c d) (C4 x y z w) = C4 (a+x) (b+y) (c+z) (d+w)
  neighborhood = Map.fromList [(C4 x y z w,1) | [x,y,z,w] <- tail (replicateM 4 [0,-1,1])]
