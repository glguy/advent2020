{-# Language QuasiQuotes, LambdaCase #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/12>

-}
module Main (main) where

import Advent.InputParser (format)
import Advent.Coord
import Data.List (foldl')

type Command = (Char, Int)

-- | The simulation tracks the current location and the vector used
-- when moving /forward/.
data Sim = Sim { here, vect :: !Coord }

-- | Apply an update function to an @a@ typed subcomponent of a
-- @s@ typed value.
type Update s a = (a -> a) -> (s -> s)

mapHere, mapVect :: Update Sim Coord
mapHere f s = s { here = f (here s) }
mapVect f s = s { vect = f (vect s) }

-- |
-- >>> :main
-- 1007
-- 41212
main :: IO ()
main =
  do inp <- [format|12 (%c%u%n)*|]
     print (walk mapHere (Sim origin east                ) inp)
     print (walk mapVect (Sim origin (move 10 east north)) inp)

walk :: Update Sim Coord -> Sim -> [Command] -> Int
walk f st xs = manhattan origin (here (foldl' (action f) st xs))

action ::
  Update Sim Coord {- ^ cardinal direction component -} ->
  Sim -> Command -> Sim
action mapCard st = \case
  ('N',   n) -> mapCard (move n north    ) st
  ('S',   n) -> mapCard (move n south    ) st
  ('E',   n) -> mapCard (move n east     ) st
  ('W',   n) -> mapCard (move n west     ) st
  ('F',   n) -> mapHere (move n (vect st)) st
  ('L',  90) -> mapVect turnLeft           st
  ('R', 270) -> mapVect turnLeft           st
  ('R',  90) -> mapVect turnRight          st
  ('L', 270) -> mapVect turnRight          st
  ('L', 180) -> mapVect turnAround         st
  ('R', 180) -> mapVect turnAround         st
  x          -> error ("Unknown command: " ++ show x)

move :: Int -> Coord -> Coord -> Coord
move n v = addCoord (scaleCoord n v)
