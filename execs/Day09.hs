{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/9>

-}
module Main (main) where

import           Advent (UVector, getParsedLines, number)
import           Data.Maybe (listToMaybe)
import qualified Data.Vector.Generic as V

main :: IO ()
main =
  do inp <- V.fromList <$> getParsedLines 9 number
     let p1 = part1 inp
     print p1

     let psums = V.scanl' (+) 0 inp
         Just (lo,hi) = search (\x y -> y-x == p1) psums
         range = V.drop lo (V.take hi inp)
     print (V.minimum range + V.maximum range)

search :: (Int -> Int -> Bool) -> UVector Int -> Maybe (Int,Int)
search p haystack =
  listToMaybe
  [(i,j) | i <- [0   .. V.length haystack - 2]
         , j <- [i+1 .. V.length haystack - 1]
         , p (haystack V.! i) (haystack V.! j)]

part1 :: UVector Int -> Int
part1 v
  | Just{} <- search (\x y -> x+y == v V.! 25) (V.take 25 v) = part1 (V.tail v)
  | otherwise = v V.! 25
