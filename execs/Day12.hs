{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/12>

-}
module Main (main) where

import Advent
import Advent.Coord

type Command = (Char, Int)

command :: Parser Command
command = (,) <$> anySingle <*> number

main :: IO ()
main =
  do inp <- getParsedLines 12 command
     print (part1 origin (C    0  1) inp)
     print (part2 origin (C (-1) 10) inp)

part1 :: Coord -> Coord -> [Command] -> Int
part1 here _ [] = manhattan origin here
part1 here dir (x:xs) =
  case x of
    ('N',   n) -> part1 (addCoord (scaleCoord n north) here) dir xs
    ('S',   n) -> part1 (addCoord (scaleCoord n south) here) dir xs
    ('E',   n) -> part1 (addCoord (scaleCoord n east ) here) dir xs
    ('W',   n) -> part1 (addCoord (scaleCoord n west ) here) dir xs
    ('F',   n) -> part1 (addCoord (scaleCoord n dir) here) dir xs
    ('L',  90) -> part1 here (turnLeft   dir) xs
    ('R',  90) -> part1 here (turnRight  dir) xs
    ('R', 270) -> part1 here (turnLeft   dir) xs
    ('L', 270) -> part1 here (turnRight  dir) xs
    ('L', 180) -> part1 here (turnAround dir) xs
    ('R', 180) -> part1 here (turnAround dir) xs
    _          -> error ("Unknown command: " ++ show x)

part2 :: Coord -> Coord -> [Command] -> Int
part2 here _ [] = manhattan origin here
part2 here dir (x:xs) =
  case x of
    ('N',   n) -> part2 here (addCoord (scaleCoord n north) dir) xs
    ('S',   n) -> part2 here (addCoord (scaleCoord n south) dir) xs
    ('E',   n) -> part2 here (addCoord (scaleCoord n east ) dir) xs
    ('W',   n) -> part2 here (addCoord (scaleCoord n west ) dir) xs
    ('F',   n) -> part2 (addCoord (scaleCoord n dir) here) dir xs
    ('L',  90) -> part2 here (turnLeft   dir) xs
    ('R',  90) -> part2 here (turnRight  dir) xs
    ('R', 270) -> part2 here (turnLeft   dir) xs
    ('L', 270) -> part2 here (turnRight  dir) xs
    ('L', 180) -> part2 here (turnAround dir) xs
    ('R', 180) -> part2 here (turnAround dir) xs
    _          -> error ("Unknown command: " ++ show x)
