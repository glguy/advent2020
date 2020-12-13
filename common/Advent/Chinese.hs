{-# Language UnboxedTuples #-}
{-|
Module      : Advent.Chinese
Description : Chinese-remainder theorem
Copyright   : 2020 Eric Mertens, 2011 Daniel Fischer, 2016-2017 Andrew Lelechenko, Carter Schonwald, Google Inc.
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Chinese (Mod(..), toMod, chinese) where

import Control.Monad (foldM)
import GHC.Integer.GMP.Internals (gcdExtInteger)

data Mod = Mod { residue, modulus :: !Integer }
  deriving (Eq, Read, Show)

-- | Construct an element of 'Mod' with a given value and modulus.
toMod ::
  Integer {- ^ residue -} ->
  Integer {- ^ modulus -} ->
  Mod     {- ^ residue mod modulus -}
toMod r m
  | m > 0     = Mod (r `mod` m) m -- needs to be `mod` to handle negative values
  | otherwise = error ("toMod: invalid modulus " ++ show m)

chinese' :: Mod -> Mod -> Maybe Mod
chinese' (Mod n1 m1) (Mod n2 m2)
  | d == 1
  = Just $! toMod (m2          * n1 * v + m1          * n2 * u) (m1          * m2)
  | (n1 - n2) `rem` d == 0
  = Just $! toMod (m2 `quot` d * n1 * v + m1 `quot` d * n2 * u) (m1 `quot` d * m2)
  | otherwise = Nothing
  where
    (# d, u #) = gcdExtInteger m1 m2
    v = (d - m1 * u) `quot` m2

chinese :: [Mod] -> Maybe Integer
chinese []     = Just 0
chinese (x:xs) = residue <$> foldM chinese' x xs

{-
Implementation of 'chinese' adapted from arithmoi-0.11.0.1 under terms of the following license.

Copyright (c) 2011 Daniel Fischer, 2016-2017 Andrew Lelechenko, Carter Schonwald, Google Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
