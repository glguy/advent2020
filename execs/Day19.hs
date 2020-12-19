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

type Rule = Either String [[Int]]

l :: Parser a -> Parser a
l p = p <* spaces

format :: Parser ([(Int,Rule)], [String])
format =
  (,) <$> endBy rule "\n" <* "\n"
      <*> endBy (many letterChar) "\n"

rule :: Parser (Int, Rule)
rule = (,) <$> decimal <* l ":" <*> rhs

rhs :: Parser Rule
rhs = Left  <$ "\"" <*> many letterChar <* "\"" <|>
      Right <$> many (l decimal) `sepBy` l "|"

main :: IO ()
main =
  do (rs,ws) <- getParsedInput 19 format

     let rules1 = IntMap.fromList rs
         rules2 = IntMap.insert  8 (Right [[42   ],[42, 8   ]])
                $ IntMap.insert 11 (Right [[42,31],[42,11,31]])
                $ rules1

     print (run rules1 ws)
     print (run rules2 ws)

run :: IntMap Rule -> [String] -> Int
run rules ws = count (not . null . R.readP_to_S parser) ws
  where
    parser  = parsers IntMap.! 0 *> R.eof

    parsers = toParser <$> rules

    toParser (Left s)    = () <$ R.string s
    toParser (Right xss) = asum [traverse_ (parsers IntMap.!) xs | xs <- xss]
