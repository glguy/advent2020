{-# Language BlockArguments, ImportQualifiedPost, NumericUnderscores, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/15>

-}
module Main (main) where

import Advent
import Control.Monad (zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (Ix, MArray, STUArray, newArray, readArray, writeArray)
import Data.Int (Int32)

-- | Type of elements in our sequence -- big enough to hold 30 million
type T = Int32

-- |
-- >>> game [10,16,6,0,1,17] 2020
-- 412
main :: IO ()
main =
  do inp <- getParsedInput 15 (decimal `sepBy` ",")
     print (game inp      2_020)
     print (game inp 30_000_000)

game ::
  [T] {- ^ initial sequence -} ->
  T   {- ^ desired position -} ->
  T   {- ^ desired element  -}
game xs n = runST
  do a <- newArray (0, maximum (n:xs)) 0
     zipWithM_ (writeArray a) (init xs) [1..]
     speak a n (fromIntegral (length xs)) (last xs)

speak ::
  STUArray s T T {- ^ position of last occurrence -} ->
  T              {- ^ desired position            -} ->
  T              {- ^ current position            -} ->
  T              {- ^ current element             -} ->
  ST s T         {- ^ desired element             -}
speak a n m x
  | m == n    = pure $! x
  | otherwise = do v <- exchange a x m
                   speak a n (m+1) (if v == 0 then 0 else m-v)

-- | Exchange element at an index with a new element returning old element.
exchange :: (Ix i, MArray a e m) => a i e -> i -> e -> m e
exchange a i x = readArray a i <* writeArray a i x
