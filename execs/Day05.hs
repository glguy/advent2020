{-# Language OverloadedStrings #-}
{-# Options_GHC -w #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/5>

-}
module Main (main) where

import Advent
import Data.List (sort)

-- |
-- >>> :main
-- 951
-- 653
main :: IO ()
main =
  do inp <- getInputLines 5
     let seatIds = map seatId inp
     print (maximum seatIds)
     print (gap (sort seatIds))

gap :: [Int] -> Int
gap (x:y:z) | x+2 == y = x+1
gap (_:xs) = gap xs

seatId :: String -> Int
seatId xs = let (r,c) = seat xs in 8*r+c

seat :: String -> (Int,Int)
seat = foldl f (0,0)
  where
    f (r,c) 'L' = (r,2*c  )
    f (r,c) 'R' = (r,2*c+1)
    f (r,c) 'F' = (2*r  ,c)
    f (r,c) 'B' = (2*r+1,c)
