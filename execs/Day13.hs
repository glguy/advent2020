{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/13>

-}
module Main (main) where

import Advent
import Advent.Chinese      (chinese)
import Data.Ord            (comparing)
import Data.List           (minimumBy)
import Control.Applicative ((<|>))
import Data.Maybe          (fromJust)

busId :: Parser (Maybe Integer)
busId = Nothing <$ "x" <|> Just <$> decimal

schedule :: Parser [Maybe Integer]
schedule = busId `sepBy` ","

format :: Parser (Integer, [Maybe Integer])
format = (,) <$> decimal  <* "\n"
             <*> schedule <* "\n"

main :: IO ()
main =
  do (t,rawBusses) <- getParsedInput 13 format
     let busses = [(i,b) | (i, Just b) <- zip [0..] rawBusses]
     print $ part1 $ minimumBy (comparing residue) [toMod (-t) b | (_,b) <- busses]
     print $ fromJust $ chinese                    [toMod (-u) b | (u,b) <- busses]

part1 :: Mod -> Integer
part1 x = residue x * modulus x
