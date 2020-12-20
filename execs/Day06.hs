{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/6>

-}
module Main (main) where

import Advent.InputParser (format)
import Control.Applicative (some)
import Data.List (intersect, union)

-- |
-- >>> :main
-- 6273
-- 3254
main :: IO ()
main =
  do inp <- [format|6 (%s%n)*&%n|]
     print (length (foldr union []   =<< inp))
     print (length (foldr1 intersect =<< inp))
