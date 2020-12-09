{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/9>

-}
module Main (main) where

import           Advent (UVector, getParsedLines, decimal)
import qualified Data.Vector.Generic as V

main :: IO ()
main =
  do inp <- V.fromList <$> getParsedLines 9 decimal

     let target = part1 inp
     print target

     let (lo,hi) = part2 target inp 0 0 0
         range = V.slice lo (hi-lo) inp
     print (V.minimum range + V.maximum range)

search :: Int -> UVector Int -> Bool
search target haystack = null
  [() | i <- [0   .. V.length haystack - 2]
      , j <- [i+1 .. V.length haystack - 1]
      , haystack V.! i + haystack V.! j == target]

part1 :: UVector Int -> Int
part1 v
  | search (v V.! 25) (V.take 25 v) = v V.! 25
  | otherwise = part1 (V.tail v)

part2 :: Int -> UVector Int -> Int -> Int -> Int -> (Int, Int)
part2 target v acc lo hi =
  case compare acc target of
    EQ -> (lo, hi)
    LT -> part2 target v (acc + v V.! hi) lo (hi+1)
    GT -> part2 target v (acc - v V.! lo) (lo+1) hi
