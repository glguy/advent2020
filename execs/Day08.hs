{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/8>

-}
module Main (main) where

import           Advent
import           Control.Applicative
import qualified Data.Vector.Generic as V
import qualified Data.Graph.Inductive as G

data Opcode = Nop | Acc | Jmp

opcode :: Parser Opcode
opcode = Nop <$ "nop" <|> Acc <$ "acc" <|> Jmp <$ "jmp"

statement :: Parser (Opcode, Int)
statement = (,) <$> opcode <* " " <*> number

main :: IO ()
main =
  do inp <- V.fromList <$> getParsedLines 8 statement
     let g       = pgmGraph inp
         p1      = G.dfs [0] (G.elfilter (0==) g)
         Just p2 = G.sp 0 (V.length inp) g

     print (pathSum inp p1)
     print (pathSum inp p2)

pathSum :: Vector (Opcode, Int) -> [Int] -> Int
pathSum inp path = sum [n | i <- path, Just (Acc, n) <- [inp V.!? i]]

pgmGraph :: Vector (Opcode, Int) -> G.Gr () Int
pgmGraph pgm =
  G.mkGraph
    [(i, ()) | i <- [0..V.length pgm]]
    [e | i <- [0..V.length pgm-1], e <- pgmEdge i (pgm V.! i)]

pgmEdge :: Int -> (Opcode, Int) -> [G.LEdge Int]
pgmEdge start (Nop, n) = [(start, start+1, 0), (start, start+n, 1)]
pgmEdge start (Jmp, n) = [(start, start+1, 1), (start, start+n, 0)]
pgmEdge start (Acc, _) = [(start, start+1, 0)]
