{-# Language BlockArguments, ScopedTypeVariables, QuasiQuotes #-}
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
import Data.Char (isDigit)
import Data.List (delete, sort)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

type Field = (String, String)
type Passport = [Field]

-- |
-- >>> :main
-- 245
-- 133
main :: IO ()
main =
  do inp <- [format|4 (%s:%s( |%n))*&%n|]
     print (count complete inp)
     print (count valid inp)

reqFields :: [String]
reqFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

complete :: Passport -> Bool
complete x = reqFields == sort (delete "cid" (map fst x))

range :: Ord a => a -> a -> a -> Bool
range lo hi x = lo <= x && x <= hi

isHex :: Char -> Bool
isHex x = isDigit x || range 'a' 'f' x

valid :: Passport -> Bool
valid x = isJust
  do guard . range (1920::Integer) 2002 =<< readMaybe =<< lookup "byr" x
     guard . range (2010::Integer) 2020 =<< readMaybe =<< lookup "iyr" x
     guard . range (2020::Integer) 2030 =<< readMaybe =<< lookup "eyr" x

     (hgtStr, hgtU) <- span isDigit <$> lookup "hgt" x
     hgt :: Integer <- readMaybe hgtStr
     guard case hgtU of
             "cm" -> range 150 193 hgt
             "in" -> range  59  76 hgt
             _    -> False

     '#':cs <- lookup "hcl" x
     guard (length cs == 6 && all isHex cs)

     ecl <- lookup "ecl" x
     guard (ecl `elem` words "amb blu brn gry grn hzl oth")

     pid <- lookup "pid" x
     guard (length pid == 9 && all isDigit pid)
