{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/6>

-}
module Main (main) where

import Advent
import Control.Applicative (some)
import Data.List (union, intersect)

main :: IO ()
main =
  do inp <- getParsedInput 6 (endBy (some letterChar) "\n" `sepBy` "\n")
     print (sum (map (length . foldr union []  ) inp))
     print (sum (map (length . foldr1 intersect) inp))
