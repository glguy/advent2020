{-# Language BlockArguments, ImportQualifiedPost, NumericUnderscores, QuasiQuotes #-}
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
import Data.Array.IO (IOUArray, getBounds, newArray_, readArray, writeArray)

-- | The array maps cup numbers (indexes) to the next cup
-- in the sequence (elements).
type Ring = IOUArray Int Int

newRing :: Int -> [Int] -> IO Ring
newRing n order =
  do a <- newArray_ (1,n)
     for_ (zip order (tail order)) \(x,y) ->
        writeArray a x y
     writeArray a (last order) (head order)
     pure a

readRing :: Ring -> Int -> Int -> IO [Int]
readRing a n i
  | n <= 0 = pure []
  | otherwise =
    do i'   <- readArray a i
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
     g1 <- readArray a cur
     g2 <- readArray a g1
     g3 <- readArray a g2

     -- find next cup and link current one to it
     nx <- readArray a g3
     writeArray a cur nx

     -- find the new destination label
     (lo,hi) <- getBounds a
     let gs = [g1,g2,g3]
         searchOrder = [cur-1, cur-2 .. lo] ++ [hi, hi-1 .. ]
         dest:_ = searchOrder \\ gs

     -- splice the group back in at dest
     writeArray a g3 =<< readArray a dest
     writeArray a dest g1

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

     x <- readArray ring 1
     y <- readArray ring x
     print (x*y)
