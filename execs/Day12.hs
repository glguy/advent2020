{-# Language LambdaCase, QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/12>

-}
module Main (main) where

import Advent.Coord
import Advent.Format (format)
import Data.List (foldl')

type Command = (D, Int)
data D = DN | DS | DE | DW | DL | DR | DF deriving (Read, Show)
pure[]

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
  do inp <- [format|12 (@D%u%n)*|]
     print (walk mapHere (Sim origin east                ) inp)
     print (walk mapVect (Sim origin (move 10 east north)) inp)

walk :: Update Sim Coord -> Sim -> [Command] -> Int
walk f st xs = manhattan origin (here (foldl' (action f) st xs))

action ::
  Update Sim Coord {- ^ cardinal direction component -} ->
  Sim -> Command -> Sim
action mapCard st = \case
  (DN,   n) -> mapCard (move n north    ) st
  (DS,   n) -> mapCard (move n south    ) st
  (DE,   n) -> mapCard (move n east     ) st
  (DW,   n) -> mapCard (move n west     ) st
  (DF,   n) -> mapHere (move n (vect st)) st
  (DL,  90) -> mapVect turnLeft           st
  (DL, 270) -> mapVect turnRight          st
  (DL, 180) -> mapVect turnAround         st
  (DL,   _) -> error "bad left turn"
  (DR,  90) -> mapVect turnRight          st
  (DR, 180) -> mapVect turnAround         st
  (DR, 270) -> mapVect turnLeft           st
  (DR,   _) -> error "bad right turn"

move :: Int -> Coord -> Coord -> Coord
move n v = addCoord (scaleCoord n v)
