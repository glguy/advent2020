{-# Language ImportQualifiedPost, OverloadedStrings, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/7>

The problem gives us a list of rules about the immediate contents
of each color of bag. We use this to compute the transitive
closure of bag contents in order to answer queries about a shiny
gold bag.

-}
module Main (main) where

import Advent (count, löb)
import Advent.Format (format)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

type Bag = String
type Rule = (String, Maybe [(Integer, String)])

------------------------------------------------------------------------

-- |
-- >>> :main
-- 268
-- 7867
main :: IO ()
main =
  do rules <- [format|7 ((%s %s)! bags contain (no other bags|(%lu (%s %s)! bag(|s))&(, )).%n)*|]
     let tc = transClosBags rules
         k = "shiny gold"
     print (count (Map.member k) tc)
     print (sum (tc Map.! k))

transClosBags :: [Rule] -> Map Bag (Map Bag Integer)
transClosBags rules =
  löb (Map.fromList [(b, expand [(c,n) | (n,c) <- fromMaybe [] xs]) | (b,xs) <- rules])

expand :: [(Bag,Integer)] -> Map Bag (Map Bag Integer) -> Map Bag Integer
expand inside tc =
  Map.unionsWith (+)
    [(n*) <$> Map.insertWith (+) b 1 (tc Map.! b) | (b,n) <- inside]
