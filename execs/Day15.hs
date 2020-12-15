{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/15>

-}
module Main (main) where

import           Advent
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

main :: IO ()
main =
  do inp <- start <$> getParsedInput 15 (decimal `sepBy` "," <* "\n")
     print $ prev $ steps (    2020-position inp-1) inp
     print $ prev $ steps (30000000-position inp-1) inp

steps 0 x = x
steps n x = steps (n-1) $! step x

data State = State { prev :: !Int, position :: !Int, seen :: !(IntMap Int) }

start :: [Int] -> State
start (x:xs) = foldl f (State x 0 IntMap.empty) xs
  where f (State a p seen) x = State x (p+1) (IntMap.insert a p seen)

step :: State -> State
step (State a p seen) =
  State (maybe 0 (p-) (IntMap.lookup a seen))
        (p+1)
        (IntMap.insert a p seen)
