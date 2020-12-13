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
import Control.Applicative
import Data.Maybe
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

busId :: Parser (Maybe Integer)
busId = Nothing <$ "x" <|> Just <$> number

schedule :: Parser [Maybe Integer]
schedule = busId `sepBy` ","

format :: Parser (Integer, [Maybe Integer])
format = (,) <$> number <* "\n" <*> schedule <* "\n"

main :: IO ()
main =
  do (t,bs) <- getParsedInput 13 format
     print $ uncurry (*) $ minimum       [ (target t b, b) |  Just b     <-     bs      ]
     print $ fromJust $ chineseRemainder [ (target u b, b) | (Just b, u) <- zip bs [0..]]

target :: Integer -> Integer -> Integer
target t b = negate t `mod` b
