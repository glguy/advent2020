{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/7>

The problem gives us a list of rules about the immediate contents
of each color of bag. We use this to compute the tnrasitive
closure of bag contents in order to answer queries about a shiny
gold bag.

-}
module Main (main) where

import           Advent (Parser, count, decimal, letterChar, sepBy1, getParsedInput, löb)
import           Control.Applicative (many, some, optional, (<|>))
import           Data.Map (Map)
import qualified Data.Map as Map

type Bag = String
type Rule = (Bag, [(Int, Bag)])

bag :: Parser Bag
bag = some letterChar <> " " <> some letterChar <* " bag" <* optional "s"

bags :: Parser (Int, Bag)
bags = (,) <$> decimal <* " " <*> bag

bagss :: Parser [(Int, Bag)]
bagss = [] <$ "no other bags" <|> bags `sepBy1` ", "

rule :: Parser Rule
rule = (,) <$> bag <* " contain " <*> bagss <* ".\n"

------------------------------------------------------------------------

-- |
-- >>> :main
-- 268
-- 7867
main :: IO ()
main =
  do rules <- getParsedInput 7 (many rule)
     let tc = transClosBags rules
     print (count (Map.member "shiny gold") tc)
     print (sum (tc Map.! "shiny gold"))

transClosBags :: [Rule] -> Map Bag (Map Bag Int)
transClosBags rules = löb (expand <$> Map.fromList rules)

expand :: [(Int,Bag)] -> Map Bag (Map Bag Int) -> Map Bag Int
expand inside tc =
  Map.unionsWith (+)
    [(n*) <$> Map.insertWith (+) b 1 (tc Map.! b) | (n,b) <- inside]
