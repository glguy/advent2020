{-# Language BlockArguments, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/18>

-}
module Main (main) where

import Advent
import Control.Applicative

data Expr = Add Expr Expr | Mul Expr Expr | Lit Integer
  deriving Show

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= go
  where
    go x =
      do f <- op
         y <- p
         go (f x y)
      <|> pure x

add, mul :: Parser (Expr -> Expr -> Expr)
add = Add <$ " + "
mul = Mul <$ " * "

aexpr :: Parser Expr -> Parser Expr
aexpr top = Lit <$> decimal <|> "(" *> top <* ")"

expr1, expr2 :: Parser Expr
expr1 = chainl1 (aexpr expr1) (add <|> mul)
expr2 = chainl1 (chainl1 (aexpr expr2) add) mul

eval :: Expr -> Integer
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Lit x  ) = x

main :: IO ()
main =
  do p1 <- getParsedLines 18 expr1
     print (sum (map eval p1))

     p2 <- getParsedLines 18 expr2
     print (sum (map eval p2))
