{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/13>

-}
module Main (main) where

import Advent
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli (chinese)

busId :: Parser (Maybe Integer)
busId = Nothing <$ "x" <|> Just <$> decimal

schedule :: Parser [Maybe Integer]
schedule = busId `sepBy` ","

format :: Parser (Integer, [Maybe Integer])
format = (,) <$> decimal  <* "\n"
             <*> schedule <* "\n"

main :: IO ()
main =
  do (t,rawBusses) <- getParsedInput 13 format
     let busses = [(i,b) | (i, Just b) <- zip [0..] rawBusses]
     print $ uncurry (*) $ minimum [intModN (-t) b | (_,b) <- busses]
     print $ fromJust $ crt        [intModN (-u) b | (u,b) <- busses]

-- | The type of integers modulo some N
type IntModN = (Integer, Integer) -- (residue, modulus)

-- | Construct an element of 'IntModN' with a given value and modulus.
intModN ::
  Integer {- ^ value   -} ->
  Integer {- ^ modulus -} ->
  IntModN {- ^ residue mod modulus -}
intModN r m = (r`mod`m, m)

-- | Chinese remainder theorem implementation that can handle
-- moduluses that aren't coprime. This finds some number @r@
-- such that for each @(r_n, m_n)@ in the input list:
-- @r = r_n mod m_n@
crt :: [IntModN] -> Maybe Integer
crt xs = fst <$> foldM chinese' (0,1) xs
  where
    chinese' (r1,m1) (r2,m2) =
      do r <- chinese (r1,m1) (r2,m2)
         Just (r, lcm m1 m2)
