{-# Language ImportQualifiedPost, OverloadedStrings #-}
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
import Control.Applicative (some)
import Data.List (delete, isPrefixOf, sortOn, transpose)

data Range = Range Int Int deriving Show

data Field = Field String [Range] deriving Show

fieldName :: Field -> String
fieldName (Field n _) = n

match1 :: Int -> Range -> Bool
match1 x (Range lo hi) = lo <= x && x <= hi

match :: Field -> Int -> Bool
match (Field _ rs) x = any (match1 x) rs

------------------------------------------------------------------------

pRange :: Parser Range
pRange = Range <$> decimal <* "-" <*> decimal

pField :: Parser Field
pField = Field <$> some (satisfy (\s -> 'a' <= s && s <= 'z' || s == ' ')) <* ": "
               <*> pRange `sepBy` " or "

pTicket :: Parser [Int]
pTicket = decimal `sepBy` ","

format :: Parser ([Field], [Int], [[Int]])
format =
  (,,) <$> pField `endBy` "\n" <* "\nyour ticket:\n"
       <*> pTicket             <* "\n\nnearby tickets:\n"
       <*> pTicket `endBy` "\n"

------------------------------------------------------------------------

main :: IO ()
main =
  do (fields, yourTicket, nearbyTickets) <- getParsedInput 16 format

     print (sum [x | xs <- nearbyTickets, x <- xs, not (any (`match` x) fields)])

     let good = [xs | xs <- nearbyTickets, all (\x -> any (`match` x) fields) xs]

     let possible
           = sortOn (length . snd)
           $ zip yourTicket
                 [ [fieldName field | field <- fields, all (match field) col]
                 | col <- transpose good]

     print (product [i | (i, name) <- head (search possible)
                       , "departure" `isPrefixOf` name])

search :: Eq b => [(a, [b])] -> [[(a,b)]]
search [] = [[]]
search ((i,names):xs) =
  [ (i,name):rest
  | name <- names
  , rest <- search [(a, delete name b) | (a,b) <- xs]
  ]
