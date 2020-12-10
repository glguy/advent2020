{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/10>

-}
module Main (main) where

import           Advent
import           Data.List
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.NumInstances ()

main :: IO ()
main =
  do adapters <- getParsedLines 10 number :: IO [Int]
     let socket = 0
     let device = maximum adapters + 3

     let jolts = socket : sort adapters ++ [device]
     let diffs = zipWith (-) (tail jolts) jolts
     print (count (3==) diffs * count (1==) diffs)

     let m :: IntMap Integer
         m = löb
           $ IntMap.insert 0 1
           $ IntMap.fromListWith (+) [ (i, rec (i-3) + rec (i-2) + rec (i-1)) | i <- device:adapters ]
         rec = IntMap.findWithDefault 0
     print (m IntMap.! device)
