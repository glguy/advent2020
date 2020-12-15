{-# Language BangPatterns, BlockArguments, OverloadedStrings #-}
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
import qualified Data.Array.ST as A
import           Control.Monad.ST (ST, runST)
import           Control.Monad (zipWithM_)

main :: IO ()
main =
  do inp <- getParsedInput 15 (decimal `sepBy` ",")
     print (game inp 2020    )
     print (game inp 30000000)

game ::
  [Int] {- ^ initial sequence -} ->
  Int   {- ^ desired position -} ->
  Int   {- ^ desired element  -}
game xs n = runST
  do a <- A.newArray (0, maximum (n:xs)) 0 :: ST s (A.STUArray s Int Int)
     zipWithM_ (A.writeArray a) (init xs) [1..]
     speak a n (length xs) (last xs)

speak ::
  A.STUArray s Int Int {- ^ position of last occurrence -} ->
  Int                  {- ^ desired position            -} ->
  Int                  {- ^ current position            -} ->
  Int                  {- ^ current element             -} ->
  ST s Int             {- ^ desired element             -}
speak a n m !x
  | m == n    = pure x
  | otherwise = do v <- swapArray a x m
                   speak a n (m+1) (if v == 0 then 0 else m-v)

-- | Replace element at an index with a new element returning old element.
swapArray :: (A.Ix i, A.MArray a e m) => a i e -> i -> e -> m e
swapArray a i x = A.readArray a i <* A.writeArray a i x
