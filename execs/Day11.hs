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
import qualified Data.Array.Unboxed as A

main :: IO ()
main =
  do inp <- getInputArray 11
     print (count ('#'==) (stable adv1 inp))
     print (count ('#'==) (stable adv2 inp))

stable :: (a -> Maybe a) -> a -> a
stable f x =
  case f x of
    Nothing -> x
    Just x' -> stable f x'

adv1 :: A.Array Coord Char -> Maybe (A.Array Coord Char)
adv1 a
  | null changes = Nothing
  | otherwise = Just $! a A.// changes
  where
    changes = [(i, v) | i <- A.range (A.bounds a), v <- valueAt i ]

    occupied = count occupied1 . neighbors

    occupied1 i =
      case arrIx a i of
        Just '#' -> True
        _        -> False

    valueAt i =
      case a A.! i of
        '#' | 4 <= occupied i -> "L"
        'L' | 0 == occupied i -> "#"
        _ -> []

adv2 :: A.Array Coord Char -> Maybe (A.Array Coord Char)
adv2 a
  | null changes = Nothing
  | otherwise = Just $! a A.// changes
  where
    changes = [(i, v) | i <- A.range (A.bounds a), v <- valueAt i ]

    occupied i = count (occupied1 i) (neighbors (C 0 0))

    occupied1 i d =
      let j = addCoord i d in
      case arrIx a j of
        Just '#' -> True
        Just '.' -> occupied1 j d
        _        -> False

    valueAt i =
      case a A.! i of
        '#' | 5 <= occupied i -> "L"
        'L' | 0 == occupied i -> "#"
        _ -> []
