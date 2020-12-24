{-# Language BlockArguments, ScopedTypeVariables, QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/4>

Passport validation

-}
module Main (main) where

import Advent
import Advent.Format (format)
import Control.Monad
import Data.Char (isDigit, isHexDigit)
import Data.List (delete, sort)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

type Passport = [(F, String)]
data F = Fbyr | Fiyr | Feyr | Fhgt | Fhcl | Fecl | Fpid | Fcid deriving (Eq, Ord, Show)
pure[]

-- |
-- >>> :main
-- 245
-- 133
main :: IO ()
main =
  do inp <- [format|4 (@F:%s( |%n))*&%n|]
     print (count complete inp)
     print (count valid inp)

reqFields :: [F]
reqFields = sort [Fbyr, Fiyr, Feyr, Fhgt, Fhcl, Fecl, Fpid]

complete :: Passport -> Bool
complete x = reqFields == sort (delete Fcid (map fst x))

range :: Ord a => a -> a -> a -> Bool
range lo hi x = lo <= x && x <= hi

valid :: Passport -> Bool
valid x = isJust
  do guard . range (1920::Integer) 2002 =<< readMaybe =<< lookup Fbyr x
     guard . range (2010::Integer) 2020 =<< readMaybe =<< lookup Fiyr x
     guard . range (2020::Integer) 2030 =<< readMaybe =<< lookup Feyr x

     (hgtStr, hgtU) <- span isDigit <$> lookup Fhgt x
     hgt :: Integer <- readMaybe hgtStr
     guard case hgtU of
             "cm" -> range 150 193 hgt
             "in" -> range  59  76 hgt
             _    -> False

     '#':cs <- lookup Fhcl x
     guard (length cs == 6 && all isHexDigit cs)

     ecl <- lookup Fecl x
     guard (ecl `elem` words "amb blu brn gry grn hzl oth")

     pid <- lookup Fpid x
     guard (length pid == 9 && all isDigit pid)
