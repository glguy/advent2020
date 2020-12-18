{-|
Module      : Main
Description : Day 18 solution using alex and happy
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/18>

-}
module Main (main) where

import Advent
import Lexer
import Parser

main :: IO ()
main =
  do inp <- lines <$> getRawInput 18
     let tokens = map alexScanTokens inp
     print (sum (map (eval . expr1) tokens))
     print (sum (map (eval . expr2) tokens))

eval :: Expr -> Integer
eval (Integer i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
