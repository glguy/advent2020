{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/3>

Sledding down a slope counting trees.

-}
module Main  where

import Advent
import Data.Vector (Vector)
import qualified Data.Vector as Vector

main :: IO ()
main =
  do inp <- Vector.fromList . map Vector.fromList <$> getInputLines 3
     print $ solve 3 1 inp
     print $ solve 1 1 inp
           * solve 3 1 inp
           * solve 5 1 inp
           * solve 7 1 inp
           * solve 1 2 inp

solve :: Int -> Int -> Vector (Vector Char) -> Int
solve dx dy vs
  = count (\(x,y) -> '#' == (vs Vector.! y) !% x)
  $ zip [0, dx ..] [0, dy .. Vector.length vs-1]

-- | Modular indexing. Indexes overflow back to the beghinning
(!%) :: Vector a -> Int -> a
v !% i = v Vector.! (i `rem` length v)
