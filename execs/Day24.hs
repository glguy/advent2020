{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/24>

-}
module Main (main) where

import Advent (cardinality, times)
import Advent.Coord (Coord, addCoord, above, below, left, right, origin)
import Advent.Format (format)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main =
  do inp <- [format|24 ((e|se|sw|w|nw|ne)!*%n)*|]
     let board = odds (map walk inp)
     print (Set.size board)
     print (Set.size (times 100 step board))

odds :: Ord a => [a] -> Set a
odds = Map.keysSet . Map.filter odd . cardinality

step :: Set Coord -> Set Coord
step board
  = Map.keysSet
  $ Map.filterWithKey rule
  $ Map.unionsWith (+)
    [Map.mapKeysMonotonic (addCoord c) neighborhood
    | c <- Set.toList board]
  where
    rule k v = v == 2 || v == 1 && Set.member k board

neighborhood :: Map Coord Int
neighborhood = Map.fromList [(move d origin,1) | d <- ["w","e","ne","se","nw","sw"]]

walk :: [String] -> Coord
walk = foldl' (flip move) origin

move :: String -> Coord -> Coord
move "w"  = left
move "e"  = right
move "ne" = above . right
move "se" = below
move "nw" = above
move "sw" = below . left
move _    = error "bad move"
