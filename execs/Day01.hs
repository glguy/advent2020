module Main where

import Advent
import Data.List (tails)

main :: IO ()
main =
  do input <- getParsedLines 1 number
     print (solve input 2)
     print (solve input 3)

solve :: [Int] -> Int -> Int
solve input n = head [product x | x <- choose n input, sum x == 2020]

choose :: Int -> [a] -> [[a]]
choose n xs
  | n <= 0 = [[]]
  | otherwise = [x:zs | x:ys <- tails xs, zs <- choose (n-1) ys]
