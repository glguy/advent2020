{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/13>

While the general case of part2 can be solved efficiently with
the Chinese remainder theorem, this local implementation is
much more straight-forward.

-}
module Main (main) where

import Advent.Format (format)
import Data.List (foldl1')

-- |
-- >>> :main
-- 3215
-- 1001569619313439
main :: IO ()
main =
  do (t,rawBusses) <- [format|13
                          %lu%n
                          (x|%lu)&,%n
                      |]
     let busses = [(i,b) | (i, Just b) <- zip [0..] rawBusses]
     print (part1 t (map snd busses))
     print (part2 busses)

toMod :: Integer -> Integer -> (Integer,Integer)
toMod x y = (x`mod`y, y)

part1 :: Integer -> [Integer] -> Integer
part1 t busses = uncurry (*) (minimum [toMod (-t) b | b <- busses])

part2 :: [(Integer,Integer)] -> Integer
part2 busses = fst (foldl1' combine [toMod (-u) b | (u,b) <- busses])
  where
    combine (n,m) (a,b)
      | n `rem` b == a = (n, lcm m b)
      | otherwise      = combine (n+m, m) (a,b)
