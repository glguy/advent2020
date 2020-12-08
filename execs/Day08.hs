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
import           Control.Monad
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Vector.Generic as V

data Opcode = Nop | Acc | Jmp

opcode :: Parser Opcode
opcode = Nop <$ "nop" <|> Acc <$ "acc" <|> Jmp <$ "jmp"

statement :: Parser (Opcode, Int)
statement = (,) <$> opcode <* " " <*> number

main :: IO ()
main =
  do inp <- V.fromList <$> getParsedLines 8 statement
     print (start inp FindStuck )
     print (start inp FindToggle)

data Mode = FindHalt | FindStuck | FindToggle deriving Eq

start :: Vector (Opcode, Int) -> Mode -> Maybe Int
start inp = run inp IntSet.empty 0 0

run :: Vector (Opcode, Int) -> IntSet -> Int -> Int -> Mode -> Maybe Int
run inp seen acc pc mode =
  case inp V.!? pc of
    _ | IntSet.member pc seen   -> acc <$ guard (mode == FindStuck)
    Nothing                     -> acc <$ guard (mode == FindHalt )
    Just cmd                    -> step cmd mode <|> alt cmd

  where
    seen' = IntSet.insert pc seen

    step (Nop, _) = run inp seen' acc     (pc+1)
    step (Acc, n) = run inp seen' (acc+n) (pc+1)
    step (Jmp, n) = run inp seen' acc     (pc+n)

    alt (Nop,n) | FindToggle <- mode = step (Jmp,n) FindHalt
    alt (Jmp,n) | FindToggle <- mode = step (Nop,n) FindHalt
    alt _                            = Nothing
