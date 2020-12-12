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
import Data.List (foldl')

type Command = (Char, Int)

data State = State { here, vect :: !Coord }

type Focus = (Coord -> Coord) -> (State -> State)

updateHere, updateVect :: Focus
updateHere f (State x y) = State (f x) y
updateVect f (State x y) = State x (f y)

command :: Parser Command
command = (,) <$> anySingle <*> number

main :: IO ()
main =
  do inp <- getParsedLines 12 command
     print (walk updateHere (State origin (C    0  1)) inp)
     print (walk updateVect (State origin (C (-1) 10)) inp)

walk :: Focus -> State -> [Command] -> Int
walk f st xs = manhattan origin (here (foldl' (action f) st xs))

action :: Focus -> State -> Command -> State
action f st ('N',   n) = f (addCoord (scaleCoord n north)) st
action f st ('S',   n) = f (addCoord (scaleCoord n south)) st
action f st ('E',   n) = f (addCoord (scaleCoord n east )) st
action f st ('W',   n) = f (addCoord (scaleCoord n west )) st
action _ st ('F',   n) = updateHere (addCoord (scaleCoord n (vect st))) st
action _ st ('L',  90) = updateVect turnLeft   st
action _ st ('R',  90) = updateVect turnRight  st
action _ st ('R', 270) = updateVect turnLeft   st
action _ st ('L', 270) = updateVect turnRight  st
action _ st ('L', 180) = updateVect turnAround st
action _ st ('R', 180) = updateVect turnAround st
action _ _  x          = error ("Unknown command: " ++ show x)
