{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/11>

-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Data.Maybe (mapMaybe)
import qualified Data.Array.Unboxed as A

type Seating   = A.UArray Coord Char
type Neighbors = A.Array Coord [Coord]

main :: IO ()
main =
  do inp <- getInputArray 11
     let run f = print (count ('#'==) (A.elems (stable f inp)))
     run (adv 4 (adjacent inp))
     run (adv 5 (lineOfSight inp))

-- | Repeatedly apply the function until it returns 'Nothing'. Return the
-- argument that returned 'Nothing'.
stable :: (a -> Maybe a) -> a -> a
stable f x = maybe x (stable f) (f x)

-- | Immediate neighbors used in part 1
adjacent :: Seating -> Neighbors
adjacent a = A.listArray b [filter (A.inRange b) (neighbors i) | i <- A.range b]
  where
    b = A.bounds a

-- | Line of sight neighbors used in part 2
lineOfSight :: Seating -> Neighbors
lineOfSight a = A.listArray b [mapMaybe (look i) (neighbors origin) | i <- A.range b]
  where
    b = A.bounds a
    look i d =
      do let j = addCoord i d
         v <- arrIx a j
         case v of
           '.' -> look j d
           _   -> Just j

-- | Advance the seating grid one timestep using a configurable
-- threshold for seats becoming unoccupied, a precomputed neighborhood,
-- and the current seating chart. Return 'Nothing' when nothing changes.
adv ::
  Int           {- ^ occupied neighbor threshold      -} ->
  Neighbors     {- ^ neighborhood for each coordinate -} ->
  Seating       {- ^ current seating grid             -} ->
  Maybe Seating {- ^ updated seating grid             -}
adv t ns a
  | null changes = Nothing
  | otherwise    = Just $! a A.// changes
  where
    changes = [(i, v) | i <- A.range (A.bounds a), v <- valueAt i]

    -- returns True when /at least/ n neighbors are occupied
    occupied :: Int -> Coord -> Bool
    occupied n i = occupied1 n (ns A.! i)

    occupied1 0 _  = True
    occupied1 _ [] = False
    occupied1 n (i:is) =
      case a A.! i of
        '#' -> occupied1 (n-1) is
        _   -> occupied1 n is

    valueAt i =
      case a A.! i of
        '#' | occupied t i       -> "L"
        'L' | not (occupied 1 i) -> "#"
        _ -> []
