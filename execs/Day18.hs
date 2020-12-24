{-# Language BlockArguments, ViewPatterns, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/18>

-}
module Main (main) where

import Advent (getInputLines)
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

-- |
-- >>> :main
-- 14208061823964
-- 320536571743074
main :: IO ()
main =
  do inp <- getInputLines 18
     print (sum (map (run expr1) inp))
     print (sum (map (run expr2) inp))

run :: ReadP a -> String -> a
run p = fst . head . readP_to_S (skipSpaces *> p <* eof)

l :: Char -> ReadP ()
l c = char c *> skipSpaces

add, mul :: ReadP (Integer -> Integer -> Integer)
add = (+) <$ l '+'
mul = (*) <$ l '*'

number :: ReadP Integer
number = read <$> munch1 isDigit <* skipSpaces

aexpr :: ReadP Integer -> ReadP Integer
aexpr top = number <|> between (l '(') (l ')') top

expr1, expr2 :: ReadP Integer
expr1 = chainl1 (aexpr expr1) (add <|> mul)
expr2 = chainl1 (chainl1 (aexpr expr2) add) mul
