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
import Data.Char (digitToInt, isDigit)

data Expr = Add Expr Expr | Mul Expr Expr | Lit Integer
  deriving Show

eval :: Expr -> Integer
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Lit x  ) = x

main :: IO ()
main =
  do p1 <- getParsedLines 18 (spaces *> expr1)
     print (sum (map eval p1))

     p2 <- getParsedLines 18 (spaces *> expr2)
     print (sum (map eval p2))

     -- redo the problem with shunting-yard
     inp <- getInputLines 18
     print (sum (map (eval . shunt p1prec [] []) inp))
     print (sum (map (eval . shunt p2prec [] []) inp))

------------------------------------------------------------------------
-- parser combinators solution -----------------------------------------
------------------------------------------------------------------------

-- zero or more spaces
spaces :: Parser ()
spaces = " " *> spaces <|> pure ()

-- lexemes are responsible for consuming trailing spaces
l :: Parser a -> Parser a
l p = p <* spaces

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= go
  where go x = ((($x) <$> op <*> p) >>= go) <|> pure x

add, mul :: Parser (Expr -> Expr -> Expr)
add = Add <$ l "+"
mul = Mul <$ l "*"

aexpr :: Parser Expr -> Parser Expr
aexpr top = Lit <$> l decimal <|> l "(" *> top <* l ")"

expr1, expr2 :: Parser Expr
expr1 = chainl1 (aexpr expr1) (add <|> mul)
expr2 = chainl1 (chainl1 (aexpr expr2) add) mul

------------------------------------------------------------------------
-- Manual operator parsing ---------------------------------------------
------------------------------------------------------------------------

shunt ::
  (Char -> Int) {- ^ operator precedence -} ->
  [Expr]        {- ^ output stack        -} ->
  [Char]        {- ^ operator stack      -} ->
  String        {- ^ input string        -} ->
  Expr          {- ^ result              -}
-- parsing complete ----------------------------------------------------
shunt _ [v] [] ""                           = v
-- skip whitespace -----------------------------------------------------
shunt p vals ops (' ':str)                  = shunt p vals ops str
-- manage parentheses matching -----------------------------------------
shunt p vals ops       ('(':str)            = shunt p vals ('(':ops) str
shunt p vals ('(':ops) (')':str)            = shunt p vals ops       str
-- push onto operator stack --------------------------------------------
shunt p vals ops ('+':str) | push p ops '+' = shunt p vals ('+':ops) str
shunt p vals ops ('*':str) | push p ops '*' = shunt p vals ('*':ops) str
-- push onto value stack -----------------------------------------------
shunt p vals ops (d:str) | isDigit d        = shunt p (evalDigit d:vals) ops str
-- reduction cases -----------------------------------------------------
shunt p (y:x:vs) ('+':ops) str              = shunt p (Add x y:vs) ops str
shunt p (y:x:vs) ('*':ops) str              = shunt p (Mul x y:vs) ops str
-- error case ----------------------------------------------------------
shunt _ vals ops str = error ("Parse error: " ++ show (vals, ops, str))

push :: (Char -> Int) -> [Char] -> Char -> Bool
push _ []    _ = True
push p (o:_) c = p c > p o

evalDigit :: Char -> Expr
evalDigit = Lit . fromIntegral . digitToInt

p1prec :: Char -> Int
p1prec '(' = 0
p1prec '*' = 1
p1prec '+' = 1
p1prec x   = error ("Bad operator: " ++ [x])

p2prec :: Char -> Int
p2prec '(' = 0
p2prec '*' = 1
p2prec '+' = 2
p2prec x   = error ("Bad operator: " ++ [x])
