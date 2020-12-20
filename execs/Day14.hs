{-# Language ImportQualifiedPost, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/14>

@
>>> :{
let Right cmds = Advent.parseLines pCmd
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
      \mem[8] = 11\n\
      \mem[7] = 101\n\
      \mem[8] = 0\n"
in run1 [] IntMap.empty cmds
:}
165

>>> :{
let Right cmds = Advent.parseLines pCmd
      "mask = 000000000000000000000000000000X1001X\n\
      \mem[42] = 100\n\
      \mask = 00000000000000000000000000000000X0XX\n\
      \mem[26] = 1\n"
in run2 [] IntMap.empty cmds
:}
208

@

-}

module Main where

import Advent
import Control.Applicative
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Bits (setBit, clearBit)

data Cmd = Mask [Mask] | Mem Int Int deriving (Show)

data Mask = I | O | X deriving (Show)

pCmd :: Parser Cmd
pCmd =
  Mask <$ "mask = " <*> replicateM 36 pMask <|>
  Mem  <$ "mem[" <*> decimal <* "] = " <*> decimal

pMask :: Parser Mask
pMask = X <$ "X" <|> I <$ "1" <|> O <$ "0"

-- |
-- >>> :main
-- 17934269678453
-- 3440662844064
main :: IO ()
main =
  do inp <- getParsedLines 14 pCmd
     print (run1 [] IntMap.empty inp)
     print (run2 [] IntMap.empty inp)

-- | Simulate the computer using the 'mask1' rule.
run1 ::
  [Mask]     {- ^ initial mask       -} ->
  IntMap Int {- ^ initial memory     -} ->
  [Cmd]      {- ^ program statements -} ->
  Int
run1 _    mem []               = sum mem
run1 _    mem (Mask mask : xs) = run1 mask mem xs
run1 mask mem (Mem k v   : xs) = run1 mask mem' xs
  where
    mem' = IntMap.insert k v' mem
    v'   = mask1 v 35 mask

-- | Apply a mask where 'I' and 'O' overwrite bits.
--
-- >>> mask1 11 6 [I,X,X,X,X,O,X]
-- 73
--
-- >>> mask1 101 6 [I,X,X,X,X,O,X]
-- 101
--
-- >>> mask1 0 6 [I,X,X,X,X,O,X]
-- 64
mask1 ::
  Int {- ^ target value                   -} ->
  Int {- ^ bit index of beginning of mask -} ->
  [Mask] -> Int
mask1 acc i (I:xs) = mask1 (setBit   acc i) (i-1) xs
mask1 acc i (O:xs) = mask1 (clearBit acc i) (i-1) xs
mask1 acc i (X:xs) = mask1 acc              (i-1) xs
mask1 acc _ []     = acc

-- | Simulate the computer using the 'mask2' rule.
run2 ::
  [Mask]     {- ^ initial mask       -} ->
  IntMap Int {- ^ initial memory     -} ->
  [Cmd]      {- ^ program statements -} ->
  Int        {- ^ sum of memory      -}
run2 _    mem []               = sum mem
run2 _    mem (Mask mask : xs) = run2 mask mem xs
run2 mask mem (Mem k v   : xs) = run2 mask mem' xs
  where
    mem' = foldl' (\m_ k_ -> IntMap.insert k_ v m_) mem
         $ mask2 k 35 mask

-- | Apply a mask where 'I' overwrites and 'X' takes both bit values.
--
-- >>> mask2 42 5 [X,I,O,O,I,X]
-- [59,27,58,26]
mask2 ::
  Int {- ^ target value                   -} ->
  Int {- ^ bit index of beginning of mask -} ->
  [Mask] -> [Int]
mask2 x i (I:xs) = mask2 (setBit x i) (i-1) xs
mask2 x i (O:xs) = mask2 x (i-1) xs
mask2 x i (X:xs) = do y <- mask2 (setBit x i) (i-1) xs; [y, clearBit y i]
mask2 x _ []     = [x]
