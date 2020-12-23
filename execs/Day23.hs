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
import Data.Char
import Data.Foldable
import Data.List
import Data.Array.IO qualified as A

newRing :: Int -> [Int] -> IO (A.IOUArray Int Int)
newRing n order =
  do a <- A.newArray_ (1,n)
     for_ (zip order (tail (cycle order))) $ \(x,y) ->
        A.writeArray a x y
     pure a

readRing :: A.IOUArray Int Int -> Int -> Int -> IO [Int]
readRing _ _ 0 = pure []
readRing a i n =
  do i' <- A.readArray a i
     rest <- readRing a i' (n-1)
     pure (i : rest)

steps :: A.IOUArray Int Int -> Int -> Int -> Int -> IO ()
steps _ _ _ 0 = return ()
steps a n cur i =
  do cur' <- step a cur
     steps a n cur' (i-1)

main :: IO ()
main =
  do inp <- map digitToInt <$> [format|23 %c*%n|]
     p1 inp
     p2 inp

p1 :: [Int] -> IO ()
p1 inp =
  do ring <- newRing (length inp) inp
     steps ring (length inp) (head inp) 100
     putStrLn . concatMap show . take 9 . tail =<< readRing ring 1 9

p2 :: [Int] -> IO ()
p2 inp =
  do let sz   =  1_000_000
         iter = 10_000_000
     let inp' = take sz (inp ++ [maximum inp+1 ..])
     ring <- newRing sz inp'
     steps ring sz (head inp') iter
     print . product =<< readRing ring 1 3

step :: A.IOUArray Int Int -> Int -> IO Int
step v cur =
  do -- extract a group of three nodes
     g1 <- A.readArray v cur
     g2 <- A.readArray v g1
     g3 <- A.readArray v g2
     g4 <- A.readArray v g3
     A.writeArray v cur g4

     (lo,hi) <- A.getBounds v
     -- find the new destination label
     let gs = [cur,g1,g2,g3]
         searchOrder = [cur-1, cur-2 .. lo] ++ [hi, hi-1 .. ]
         dest:_ = searchOrder \\ gs

     -- splice the group in
     A.writeArray v g3 =<< A.readArray v dest
     A.writeArray v dest g1

     pure g4
