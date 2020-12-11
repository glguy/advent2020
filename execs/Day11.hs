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

type Grid = A.Array Coord Char

main :: IO ()
main =
  do inp <- getInputArray 11
     let run f = print (count ('#'==) (stable f inp))
     run (adv 4 (adjacent inp))
     run (adv 5 (lineOfSight inp))

-- | Repeatedly apply the function until it returns 'Nothing'. Return the
-- argument that returned 'Nothing'.
stable :: (a -> Maybe a) -> a -> a
stable f x = maybe x (stable f) (f x)

-- | Immediate neighbors used in part 1
adjacent :: Grid -> A.Array Coord [Coord]
adjacent a = A.listArray (A.bounds a)
  [filter (A.inRange (A.bounds a)) (neighbors i) | i <- A.range (A.bounds a)]

-- | Line of sight neighbors used in part 2
lineOfSight :: Grid -> A.Array Coord [Coord]
lineOfSight a = A.listArray (A.bounds a)
  [ mapMaybe (look i) (neighbors origin) | i <- A.range (A.bounds a) ]
  where
    look i d =
      do let j = addCoord i d
         v <- arrIx a j
         case v of
           '#' -> Just j
           'L' -> Just j
           _   -> look j d

adv :: Int -> A.Array Coord [Coord] -> Grid -> Maybe (A.Array Coord Char)
adv t ns a
  | null changes = Nothing
  | otherwise    = Just $! a A.// changes
  where
    changes = [(i, v) | i <- A.range (A.bounds a), v <- valueAt i ]

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
