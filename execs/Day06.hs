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

import Advent (Parser, endBy, getParsedInput, letterChar, sepBy)
import Control.Applicative (some)
import Data.List (intersect, union)

-- |
-- >>> :main
-- 6273
-- 3254
main :: IO ()
main =
  do inp <- getParsedInput 6 parser
     print (length (foldr union []   =<< inp))
     print (length (foldr1 intersect =<< inp))

-- |
-- >>> Advent.parseMaybe parser "abc\nd\n\ne\n"
-- Just [["abc","d"],["e"]]
parser :: Parser [[String]]
parser = (some letterChar `endBy` "\n") `sepBy` "\n"
