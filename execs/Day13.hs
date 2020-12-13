{-# Language OverloadedStrings #-}
{-# Options_GHC -Wno-deprecations #-}
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
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli (chineseRemainder)

busId :: Parser (Maybe Integer)
busId = Nothing <$ "x" <|> Just <$> decimal

schedule :: Parser [Maybe Integer]
schedule = busId `sepBy` ","

format :: Parser (Integer, [Maybe Integer])
format = (,) <$> decimal <* "\n" <*> schedule <* "\n"

main :: IO ()
main =
  do (t,rawBusses) <- getParsedInput 13 format
     let busses = [(i,b) | (i, Just b) <- zip [0..] rawBusses]
     print $ uncurry (*) $ minimum       [toMod (-t) b | (_,b) <- busses]
     print $ fromJust $ chineseRemainder [toMod (-u) b | (u,b) <- busses]

toMod :: Integer -> Integer -> (Integer, Integer)
toMod r m = (r`mod`m, m)
