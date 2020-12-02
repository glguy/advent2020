{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/2>

-}
module Main (main) where

import Advent
import Control.Applicative (many)

type Input = (Int, Int, Char, String)

format :: Parser Input
format = (,,,) <$> decimal <* "-" <*> decimal <* " " <*> anySingle <* ": " <*> many anySingle

main :: IO ()
main =
  do inp <- getParsedLines 2 format
     print (count p1 inp)
     print (count p2 inp)

p1 :: Input -> Bool
p1 (lo,hi,c,str) = lo <= n && n <= hi
  where n = count (c==) str

p2 :: Input -> Bool
p2 (i,j,c,str) = check i /= check j
  where check x = (str !! (x-1)) == c
