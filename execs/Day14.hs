{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/14>

-}
module Main where

import           Advent
import           Control.Applicative
import           Data.List
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Bits

data Cmd = Mask [Mask] | Mem Int Int deriving (Show)

data Mask = I | O | X deriving (Show)

pCmd :: Parser Cmd
pCmd =
  Mask <$ "mask = " <*> many pMask <|>
  Mem <$ "mem[" <*> decimal <* "] = " <*> decimal

pMask :: Parser Mask
pMask = X <$ "X" <|> I <$ "1" <|> O <$ "0"

main :: IO ()
main =
  do inp <- getParsedLines 14 pCmd
     print (sum (run1 [] IntMap.empty inp))
     print (sum (run2 [] IntMap.empty inp))

run1 :: [Mask] -> IntMap Int -> [Cmd] -> IntMap Int
run1 _    mem []               = mem
run1 _    mem (Mask mask : xs) = run1 mask mem xs
run1 mask mem (Mem k v   : xs) = run1 mask mem' xs
  where
    mem' = IntMap.insert k v' mem
    v'   = msk1 v 35 mask

msk1 :: Int -> Int -> [Mask] -> Int
msk1 acc i (I:xs) = msk1 (setBit   acc i) (i-1) xs
msk1 acc i (O:xs) = msk1 (clearBit acc i) (i-1) xs
msk1 acc i (X:xs) = msk1 acc              (i-1) xs
msk1 acc _ []     = acc

run2 :: [Mask] -> IntMap Int -> [Cmd] -> IntMap Int
run2 _    mem []               = mem
run2 _    mem (Mask mask : xs) = run2 mask mem xs
run2 mask mem (Mem k v   : xs) = run2 mask mem' xs
  where
    mem' = foldl' (\m_ k_ -> IntMap.insert k_ v m_) mem ks
    ks = msk2 [k] 35 mask

msk2 :: [Int] -> Int -> [Mask] -> [Int]
msk2 acc i (I:xs) = msk2 (map (`setBit` i) acc) (i-1) xs
msk2 acc i (O:xs) = msk2 acc (i-1) xs
msk2 acc i (X:xs) = msk2 (do a <- acc; [setBit a i, clearBit a i]) (i-1) xs
msk2 acc _ []     = acc
