{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/16>

-}
module Main (main) where

import           Advent
import           Control.Applicative
import           Data.List
import           Data.Char
import           Data.Ord

data Range = Range Int Int deriving Show

data Field = Field String Range Range deriving Show

pRange :: Parser Range
pRange = Range <$> decimal <* "-" <*> decimal

pField :: Parser Field
pField = Field <$> some (satisfy (\s -> isAlpha s || s == ' ')) <* ": " <*> pRange <* " or " <*> pRange

pTicket :: Parser [Int]
pTicket = decimal `sepBy` ","

format :: Parser ([Field], [Int], [[Int]])
format =
  (,,) <$> pField `endBy` "\n" <* "\nyour ticket:\n"
       <*> pTicket             <* "\n\nnearby tickets:\n"
       <*> pTicket `endBy` "\n"

main :: IO ()
main =
  do (fields, yourTicket, nearbyTickets) <- getParsedInput 16 format

     print (sum [ x | xs <- nearbyTickets, x <- xs
                    , not (any (\field -> match field x) fields)])

     let good = [ xs | xs <- nearbyTickets
                     , all (\x -> any (\field -> match field x) fields) xs]

     let columns
           = sortBy (comparing (\(_,x) -> count (\field -> all (match field) x) fields))
           $ zip [0::Int ..] (transpose good)

     let mapping = head (search fields columns)

     print (product [yourTicket !! i
                    | (i, Field name _ _) <- mapping, "departure" `isPrefixOf` name])

match1 :: Range -> Int -> Bool
match1 (Range lo hi) x = lo <= x && x <= hi

match :: Field -> Int -> Bool
match (Field _ x y) z = match1 x z || match1 y z

search :: [Field] -> [(Int, [Int])] -> [[(Int,Field)]]
search []     []         = [[]]
search fields ((i,x):xs) =
  [ (i, field):rest
  | (field, fields') <- pickOne fields
  , all (match field) x
  , rest <- search fields' xs
  ]
search _ _ = []
