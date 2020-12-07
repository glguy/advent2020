{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/7>

-}
module Main (main) where

import           Advent (Parser, count, decimal, letterChar, sepBy, getParsedLines)
import           Control.Applicative (some, optional, (<|>))
import           Data.Map (Map)
import qualified Data.Map as Map

type Bag = (String, String)

target :: Bag
target = ("shiny","gold")

word :: Parser String
word = some letterChar

bag :: Parser Bag
bag = (,) <$> word <* " " <*> word <* " bag" <* optional "s"

amount :: Parser Int
amount = 0 <$ "no" <|> decimal

rule :: Parser (Bag, [(Int, Bag)])
rule = (,) <$> bag <* " contain " <*>
       ([] <$ "no other bags" <|> sepBy ((,) <$> amount <* " " <*> bag) ", ")
       <* "."

main :: IO ()
main =
  do inp <- getParsedLines 7 rule
     let m = contents inp
     print (count (Map.member target) m)
     print (sum (m Map.! target))

contents :: [(Bag, [(Int, Bag)])] -> Map Bag (Map Bag Int)
contents rules = m
  where
    m = expand <$> Map.fromList rules

    expand inside
      = Map.unionsWith (+)
      [ fmap (n*)
      $ Map.insertWith (+) sub 1
      $ m Map.! sub
      | (n, sub) <- inside]

