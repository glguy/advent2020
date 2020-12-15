{-# Language OverloadedStrings, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/8>

-}
module Main (main) where

import Advent
import Control.Applicative
import Data.Maybe (fromJust, mapMaybe)
import Data.Graph.Inductive qualified as G

-- | Programs are expressed as control-flow graphs.
--
-- Nodes are instructions in the program.
--
-- Nodes are labeled with the accumulator-effect of executing that instruction.
--
-- Edges capture control flow between instructions.
--
-- Edges are labeled with the /cost/ of taking that edge. It costs @1@ to
-- take a control path generated from a toggled instruction.
type Cfg = G.Gr Int Int

------------------------------------------------------------------------

data Opcode = Nop | Acc | Jmp

opcode :: Parser Opcode
opcode = Nop <$ "nop" <|> Acc <$ "acc" <|> Jmp <$ "jmp"

instruction :: Parser (Opcode, Int)
instruction = (,) <$> opcode <* " " <*> number

------------------------------------------------------------------------

-- |
-- >>> :main
-- 1200
-- 1023
main :: IO ()
main =
  do cfg <- pgmToCfg <$> getParsedLines 8 instruction
     print (pathSum cfg (G.dfs [0] (G.elfilter (0==) cfg)))
     print (pathSum cfg (fromJust (G.sp 0 (G.noNodes cfg-1) cfg)))

-- | Sum of node labels along a path in a graph.
pathSum :: Cfg -> G.Path -> Int
pathSum inp path = sum (mapMaybe (G.lab inp) path)

pgmToCfg :: [(Opcode, Int)] -> Cfg
pgmToCfg pgm =
  G.mkGraph
    (zip [0..] (map accEffect pgm ++ [0]))
    (concat (zipWith edge [0..] pgm))

accEffect :: (Opcode, Int) -> Int
accEffect (Acc, n) = n
accEffect _        = 0

edge :: Int -> (Opcode, Int) -> [G.LEdge Int]
edge i (Nop, n) = [(i, i+1, 0), (i, i+n, 1)]
edge i (Jmp, n) = [(i, i+n, 0), (i, i+1, 1)]
edge i (Acc, _) = [(i, i+1, 0)]
