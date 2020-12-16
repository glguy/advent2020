{-# Language ImportQualifiedPost, OverloadedStrings #-}
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
import Data.List (delete, isPrefixOf, sortBy, transpose)
import Data.Ord (comparing)

data Range = Range Int Int deriving Show

data Field = Field String Range Range deriving Show

fieldName :: Field -> String
fieldName (Field n _ _) = n

match1 :: Range -> Int -> Bool
match1 (Range lo hi) x = lo <= x && x <= hi

match :: Field -> Int -> Bool
match (Field _ x y) z = match1 x z || match1 y z

------------------------------------------------------------------------

pRange :: Parser Range
pRange = Range <$> decimal <* "-" <*> decimal

pField :: Parser Field
pField = Field <$> some (satisfy (\s -> 'a' <= s && s <= 'z' || s == ' ')) <* ": "
               <*> pRange <* " or " <*> pRange

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

     let good = yourTicket
              : [xs | xs <- nearbyTickets, all (\x -> any (`match` x) fields) xs]

     let possible
           = sortBy (comparing (length . snd))
           $ zip yourTicket
                 [ [fieldName field | field <- fields, all (match field) col]
                 | col <- transpose good]

     print (product [i | (i, name) <- backprop possible
                       , "departure" `isPrefixOf` name])

backprop :: Eq b => [(a, [b])] -> [(a,b)]
backprop xs = löb [pick a bs | (a,bs) <- xs]

pick :: Eq b => a -> [b] -> [(a,b)] -> (a,b)
pick a [b] _        = (a,b)
pick a bs ((_,y):z) = pick a (delete y bs) z
pick _ _  _         = error "backprop failed"

{-
search :: Eq b => [(a, [b])] -> [[(a,b)]]
search [] = [[]]
search ((i,names):xs) =
  [ (i,name):rest
  | name <- names
  , rest <- search [(a, delete name b) | (a,b) <- xs]
  ]
-}
