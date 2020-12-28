{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/21>

-}
module Main (main) where

import Advent (count, uniqueAssignment)
import Advent.Format (format)
import Data.List (intercalate, sort)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- |
-- >>> :main
-- 2517
-- rhvbn,mmcpg,kjf,fvk,lbmt,jgtb,hcbdb,zrb
main :: IO ()
main =
  do inp <- [format|21 (%s&  %(contains %s&(, )%)%n)*|]
     let solns    = uniqueAssignment (toConstraints inp)
         badFoods = Set.fromList [food | soln <- solns, (_,food) <- soln]

     print (count (`Set.notMember` badFoods) (concatMap fst inp))
     putStrLn (intercalate "," (map snd (sort (head solns))))

toConstraints :: (Ord a, Ord b) => [([a],[b])] -> [(b, Set a)]
toConstraints inp =
  Map.assocs $ Map.fromListWith Set.intersection
    [(y, Set.fromList xs) | (xs, ys) <- inp, y <- ys]
