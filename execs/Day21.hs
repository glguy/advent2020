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

import Advent.Format (format)
import Data.List
import Data.Map qualified as Map

main :: IO ()
main =
  do inp <- [format|21 (%s&  %(contains %s&(, )%)%n)*|]
     let bad = map snd (sort (search inp))
     print (length (concatMap ((\\ bad) . fst) inp))
     putStrLn (intercalate "," bad)

search :: [([String], [String])] -> [(String,String)]
search []  = []
search inp =
  case limits of
    []      -> error "bummer"
    (k,v):_ -> (k,v) : search [(delete v x, delete k y)
                              | (x,y) <- inp
                              , let y' = delete k y
                              , not (null y') ]
  where
    limits
      = Map.toList
      $ Map.mapMaybe single
      $ Map.fromListWith intersect
        [ (al, foods) | (foods, als) <- inp, al <- als]

single :: [a] -> Maybe a
single [x] = Just x
single _   = Nothing
