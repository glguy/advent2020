{-# Language DataKinds, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/25>

Brute-forcing this took me about 6 seconds, but using math makes it instant.

-}
module Main (main) where

import Advent.Format (format)
import Math.NumberTheory.Moduli

type Modulus = 20201227

main :: IO ()
main =
  do (pub1,pub2) <- [format|25 %u%n%u%n|]
     print (solve pub1 pub2)

solve :: Int -> Int -> Integer
solve pub1 pub2 = getVal (public2 ^% privateLoop)
  where
    public1      = fromIntegral pub1 :: Mod Modulus
    public2      = fromIntegral pub2 :: Mod Modulus
    Just m       = cyclicGroup
    Just subject = isPrimitiveRoot m 7
    Just public' = isMultElement public1
    privateLoop  = discreteLogarithm m subject public'
