{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/3>

Sledding down a slope counting trees.

-}
module Main  where

import Advent

main :: IO ()
main =
  do inp <- map (concat . repeat) <$> getInputLines 3
     let hits = count (\x -> head x == '#')
     print $ hits (zipWith drop [0,3..] inp)
     print $ hits (zipWith drop [0,1..] inp)
           * hits (zipWith drop [0,3..] inp)
           * hits (zipWith drop [0,5..] inp)
           * hits (zipWith drop [0,7..] inp)
           * hits (zipWith drop [0,1..] (everyOther inp))

everyOther :: [a] -> [a]
everyOther (x:_:xs) = x : everyOther xs
everyOther [x]= [x]
everyOther _ = error "bad input"
