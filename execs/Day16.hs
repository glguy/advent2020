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

data Range = Range { lo, hi :: Int } deriving Show

data Field = Field { fieldName :: String, fieldRange :: [Range] } deriving Show

match1 :: Int -> Range -> Bool
match1 x r = lo r <= x && x <= hi r

match :: Field -> Int -> Bool
match field x = any (match1 x) (fieldRange field)

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

     -- print sum of invalid fields
     print $ sum [x | xs <- nearbyTickets, x <- xs, not (any (`match` x) fields)]

     let goodTickets = [xs | xs <- nearbyTickets, all (\x -> any (`match` x) fields) xs]

         possibleFields col = [fieldName field | field <- fields, all (match field) col]

         allCandidates = [possibleFields col | col <- transpose goodTickets]

         -- pair up my ticket's field values with the candidate field names
         constraints :: [(Int, [String])]
         constraints = sortOn (length . snd) (zip yourTicket allCandidates)

     print $ product [i | (i, name) <- head (search constraints)
                        , "departure" `isPrefixOf` name]

-- | Find an way to choose a single @b@ from each list such that
-- all the chosen elements are unique.
search :: Eq b => [(a, [b])] -> [[(a,b)]]
search [] = [[]]
search ((i,names):xs) =
  [ (i,name):rest
  | name <- names
  , rest <- search [(a, delete name b) | (a,b) <- xs]
  ]
