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

main :: IO ()
main =
  do (xs,ys) <- [format|22 Player 1:%n(%u%n)*%nPlayer 2:%n(%u%n)*|]
     print (score (play1 (Seq.fromList xs) (Seq.fromList ys)))
     print (either score score (play2 Set.empty (Seq.fromList xs) (Seq.fromList ys)))

type Deck = Seq Int

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

play1 :: Deck -> Deck -> Deck
play1 Seq.Empty xs = xs
play1 xs Seq.Empty = xs
play1 (x Seq.:<| xs) (y Seq.:<| ys)
  | x > y     = play1 (give xs x y) ys
  | otherwise = play1 xs (give ys y x)

play2 :: Set (UVector Int) -> Deck -> Deck -> Either Deck Deck
play2 _ Seq.Empty xs = Right xs
play2 _ xs Seq.Empty = Left xs
play2 seen xxs@(x Seq.:<| xs) yys@(y Seq.:<| ys)
  | Set.member here seen = Left xs

  | x <= Seq.length xs, y <= Seq.length ys =
      case play2 Set.empty (Seq.take x xs) (Seq.take y ys) of
        Left {} -> play2 seen1 (give xs x y) ys
        Right{} -> play2 seen1 xs (give ys y x)

  | x > y     = play2 seen1 (give xs x y) ys
  | otherwise = play2 seen1 xs (give ys y x)
  where
    here = characterize xxs yys
    seen1 = Set.insert here seen

characterize :: Deck -> Deck -> UVector Int
characterize xs ys = V.fromList (toList xs ++ [-1] ++ toList ys)

give :: Deck -> Int -> Int -> Deck
give a b c = a Seq.|> b Seq.|> c
