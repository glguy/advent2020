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

import Advent
import Control.Applicative
import Data.Char (isDigit)

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
     print (sum (map (eval . shuntParser p1prec) inp))
     print (sum (map (eval . shuntParser p2prec) inp))

------------------------------------------------------------------------
-- parser combinators solution -----------------------------------------
------------------------------------------------------------------------

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

-- | Parser mode.
data Mode
  = N -- | Expecting a number or @(@
  | O -- | Expecting an operator
  deriving Show

shuntParser :: (Char -> Int) -> String -> Expr
shuntParser p = shunt p N [] []

shunt ::
  (Char -> Int) {- ^ operator precedence -} ->
  Mode          {- ^ parser mode         -} ->
  [Expr]        {- ^ output stack        -} ->
  [Char]        {- ^ operator stack      -} ->
  String        {- ^ input string        -} ->
  Expr          {- ^ result              -}
-- parsing complete ----------------------------------------------------
shunt _ O [v]  []  ""                         = v
-- skip whitespace -----------------------------------------------------
shunt p m vals ops (' ':str)                  = shunt p m vals ops str
-- manage parentheses matching -----------------------------------------
shunt p O vals ('(':ops) (')':str)            = shunt p O vals ops       str
shunt p N vals ops       ('(':str)            = shunt p N vals ('(':ops) str
-- push onto operator stack --------------------------------------------
shunt p O vals ops ('+':str) | push p ops '+' = shunt p N vals ('+':ops) str
shunt p O vals ops ('*':str) | push p ops '*' = shunt p N vals ('*':ops) str
-- push literals onto output stack -------------------------------------
shunt p N vals ops (lit -> Just (n,str))      = shunt p O (Lit n:vals) ops str
-- reduction cases -----------------------------------------------------
shunt p O (y:x:vs) ('+':ops) str              = shunt p O (Add x y:vs) ops str
shunt p O (y:x:vs) ('*':ops) str              = shunt p O (Mul x y:vs) ops str
-- error case ----------------------------------------------------------
shunt _ m vals ops str = error ("Parse error: " ++ show (m, vals, ops, str))

lit :: String -> Maybe (Integer, String)
lit str =
  case span isDigit str of
    ([],_)    -> Nothing
    (ds,rest) -> Just (read ds, rest)

push :: (Char -> Int) -> [Char] -> Char -> Bool
push _ []    _ = True
push p (o:_) c = p c > p o

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
