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
  do (pub1,pub2) <- [format|25 %lu%n%lu%n|]
     let public1      = fromIntegral pub1 :: Mod Modulus
     let public2      = fromIntegral pub2 :: Mod Modulus
     let Just m       = cyclicGroup :: Maybe (CyclicGroup Integer Modulus)
     let Just subject = isPrimitiveRoot m 7
     let Just public' = isMultElement (fromInteger pub1)
     let privateLoop  = discreteLogarithm m subject public'
     print (getVal (public2 ^% privateLoop))
