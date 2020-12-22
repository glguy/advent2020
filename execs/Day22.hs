{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/22>

-}
module Main (main) where

import Advent
import Advent.Format (format)
import Data.Foldable
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Generic qualified as V

-- |
-- >>> :main
-- 35818
-- 34771
main :: IO ()
main =
  do (xs,ys) <- [format|22 Player 1:%n(%u%n)*%nPlayer 2:%n(%u%n)*|]
     let p1 = Seq.fromList xs
     let p2 = Seq.fromList ys
     print (score (play1 p1 p2))
     print (score (snd (play2 Set.empty p1 p2)))

type Deck = Seq Int

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

play1 :: Deck -> Deck -> Deck
play1 Seq.Empty xs = xs
play1 xs Seq.Empty = xs
play1 (x Seq.:<| xs) (y Seq.:<| ys)
  | x > y     = play1 (give xs x y) ys
  | otherwise = play1 xs (give ys y x)

play2 :: Set (UVector Int) -> Deck -> Deck -> (Bool, Deck)
play2 _ Seq.Empty xs = (False, xs)
play2 _ xs Seq.Empty = (True, xs)
play2 seen xxs@(x Seq.:<| xs) yys@(y Seq.:<| ys)

  -- p1 wins loops
  | Set.member here seen = (True, xxs)

  -- recursive game
  | x <= Seq.length xs, y <= Seq.length ys
  , let x' = Seq.take x xs
  , let y' = Seq.take y ys
  , let x1 = maximum x' -- best p1 card
  , let y1 = maximum x' -- best p2 card

    -- if P1 has the high card that can't be lost to a
    -- recursive game then he will always eventually win:
    -- He'll never lose that card and wins in the case of
    -- a loop
  = if x1 > y1 && x1 >= (x+y) || fst (play2 Set.empty x' y')
      then p1win
      else p2win

  -- regular game
  | x > y     = p1win
  | otherwise = p2win

  where
    here  = characterize xxs yys
    seen1 = Set.insert here seen
    p1win = play2 seen1 (give xs x y) ys
    p2win = play2 seen1 xs (give ys y x)

characterize :: Deck -> Deck -> UVector Int
characterize xs ys = V.fromList (toList xs ++ [-1] ++ toList ys)

give :: Deck -> Int -> Int -> Deck
give a b c = a Seq.|> b Seq.|> c
