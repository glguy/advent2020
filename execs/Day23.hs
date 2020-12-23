{-# Language NumericUnderscores, BlockArguments, ImportQualifiedPost, OverloadedStrings, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/23>

-}
module Main (main) where

import Advent.Format (format)
import Data.Char (digitToInt)
import Data.Foldable (for_)
import Data.List ((\\))
import Data.Array.IO qualified as A

-- | The array maps cup numbers (indexes) to the next cup
-- in the sequence (elements).
type Ring = A.IOUArray Int Int

newRing :: Int -> [Int] -> IO Ring
newRing n order =
  do a <- A.newArray_ (1,n)
     for_ (zip order (tail order)) $ \(x,y) ->
        A.writeArray a x y
     A.writeArray a (last order) (head order)
     pure a

readRing :: Ring -> Int -> Int -> IO [Int]
readRing a n i
  | n <= 0 = pure []
  | otherwise =
    do i'   <- A.readArray a i
       rest <- readRing a (n-1) i'
       pure (i : rest)

chain :: Int -> (a -> IO a) -> a -> IO ()
chain n f x
  | n <= 0    = pure ()
  | otherwise = chain (n-1) f =<< f x

-- |
-- :main
-- 47382659
-- 42271866720
main :: IO ()
main =
  do inp <- map digitToInt <$> [format|23 %c*%n|]
     p1 inp
     p2 inp

step :: Ring -> Int -> IO Int
step a cur =
  do -- extract a group of three cups
     g1 <- A.readArray a cur
     g2 <- A.readArray a g1
     g3 <- A.readArray a g2

     -- find next cup and link current one to it
     nx <- A.readArray a g3
     A.writeArray a cur nx

     -- find the new destination label
     (lo,hi) <- A.getBounds a
     let gs = [g1,g2,g3]
         searchOrder = [cur-1, cur-2 .. lo] ++ [hi, hi-1 .. ]
         dest:_ = searchOrder \\ gs

     -- splice the group back in at dest
     A.writeArray a g3 =<< A.readArray a dest
     A.writeArray a dest g1

     pure nx

p1 :: [Int] -> IO ()
p1 inp =
  do ring <- newRing (length inp) inp
     chain 100 (step ring) (head inp)

     xs <- readRing ring (length inp) 1
     putStrLn (concatMap show (tail xs))

p2 :: [Int] -> IO ()
p2 inp =
  do let sz   =  1_000_000
         iter = 10_000_000
         inp' = take sz (inp ++ [maximum inp+1 ..])

     ring <- newRing sz inp'
     chain iter (step ring) (head inp')

     x <- A.readArray ring 1
     y <- A.readArray ring x
     print (x*y)
