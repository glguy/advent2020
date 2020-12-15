{-# Language OverloadedStrings, BangPatterns #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/15>

-}
module Main (main) where

import           Advent
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

main :: IO ()
main =
  do inp <- getParsedInput 15 (decimal `sepBy` "," <* "\n")
     let start = (last inp, length inp-1, IntMap.fromList (zip (init inp) [0..]))
     print $ (\(x,_,_)->x) $ searchN (    2020-length inp) start
     print $ (\(x,_,_)->x) $ searchN (30000000-length inp) start

searchN 0 !x = x
searchN n !x = searchN (n-1) (search x)

search (!a,!p,!seen) =
  case IntMap.lookup a seen of
    Nothing -> (0  , p+1, IntMap.insert a p seen)
    Just x  -> (p-x, p+1, IntMap.insert a p seen)
