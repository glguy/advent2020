{-# Language ImportQualifiedPost, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/19>

-}
module Main (main) where

import Advent
import Control.Applicative
import Data.Foldable (asum, traverse_)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Text.ParserCombinators.ReadP qualified as R

type Rule = Either Char [[Int]]

format :: Parser ([(Int,Rule)], [String])
format = (,) <$> rule `endBy` "\n" <* "\n" <*> word `endBy` "\n"

word :: Parser String
word = some letterChar

rule :: Parser (Int, Rule)
rule = (,) <$> decimal <* l ":" <*> rhs

rhs :: Parser Rule
rhs = Left  <$ "\"" <*> letterChar <* "\"" <|>
      Right <$> many (l decimal) `sepBy` l "|"

main :: IO ()
main =
  do (rs,ws) <- getParsedInput 19 format

     let rules1 = IntMap.fromList rs
         rules2 = IntMap.insert  8 (Right [[42],[42,8]])
                $ IntMap.insert 11 (Right [[42,31],[42,11,31]])
                $ rules1

     let parsers1 = löb (expand <$> rules1)
     let parsers2 = löb (expand <$> rules2)

     let run ps = print $ count (not . null . R.readP_to_S (ps IntMap.! 0 *> R.eof)) ws
     run parsers1
     run parsers2

expand :: Either Char [[Int]] -> IntMap (R.ReadP ()) -> R.ReadP ()
expand (Left c)    _ = () <$ R.char c
expand (Right xss) m = asum [traverse_ (m IntMap.!) xs | xs <- xss]
