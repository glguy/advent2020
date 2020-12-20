{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-# Options_GHC -w #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/16>

-}
module Main (main) where

import Advent
import Advent.InputParser (format)
import Control.Applicative (some)
import Data.List ((\\), isPrefixOf, sortOn, transpose)

type Range = (Integer, Integer)
type Field = ([String], [Range])

match1 :: Integer -> Range -> Bool
match1 x (lo,hi) = lo <= x && x <= hi

match :: Field -> Integer -> Bool
match (_,range) x = any (match1 x) range

------------------------------------------------------------------------

-- |
-- >>> :main
-- 25916
-- 2564529489989
main :: IO ()
main =
  do (fields, yourTicket, nearbyTickets) <-
       [format|16
         (%s& : (%lu-%lu)&( or )%n)*%n
         your ticket:%n
         (%lu&,)%n
         %n
         nearby tickets:%n
         (%lu&,%n)*
       |]

     -- print sum of invalid fields
     print $ sum [x | xs <- nearbyTickets, x <- xs, not (any (`match` x) fields)]

     let goodTickets = [xs | xs <- nearbyTickets, all (\x -> any (`match` x) fields) xs]

         possibleFields col = [fst field | field <- fields, all (match field) col]

         allCandidates = [possibleFields col | col <- transpose goodTickets]

         -- pair up my ticket's field values with the candidate field names
         constraints = sortOn (length . snd) (zip yourTicket allCandidates)

     print $ product [i | (i, name) <- head (search [] constraints)
                        , ["departure"] `isPrefixOf` name]

-- | Find an way to choose a single @b@ from each list such that
-- all the chosen elements are unique.
search :: Eq b => [b] -> [(a, [b])] -> [[(a,b)]]
search _    []          = [[]]
search seen ((a,bs):xs) = [(a,b):rest | b <- bs\\seen, rest <- search (b:seen) xs]
