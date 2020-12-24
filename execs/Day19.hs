{-# Language ImportQualifiedPost, OverloadedStrings, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/19>

-}
module Main (main) where

import Advent (count, löb)
import Advent.Format (format)
import Data.Foldable (asum, traverse_)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Text.ParserCombinators.ReadP (ReadP, eof, readP_to_S, string)

-- | Rules either match a literal string, or match a sum
-- of product of sub-rules.
type Rule = Either String [[Int]]

------------------------------------------------------------------------

-- | Print answers.
--
-- >>> :main
-- 180
-- 323
main :: IO ()
main =
  do (rs,ws) <- [format|19 (%u: ("%s"|%u& &( %| ))%n)*%n(%s%n)*|]
     let rules1 = IntMap.fromList rs
         rules2 = IntMap.insert  8 (Right [[42   ],[42, 8   ]])
                $ IntMap.insert 11 (Right [[42,31],[42,11,31]])
                $ rules1
     print (run rules1 ws)
     print (run rules2 ws)

run ::
  IntMap Rule {- ^ parse rules                      -} ->
  [String]    {- ^ input strings                    -} ->
  Int         {- ^ number of matching input strings -}
run rules ws = count (not . null . readP_to_S parser) ws
  where
    parser = löb (ruleParser <$> rules) IntMap.! 0 *> eof

ruleParser :: Rule -> IntMap (ReadP ()) -> ReadP ()
ruleParser (Left  s  ) _   = () <$ string s
ruleParser (Right xss) sub = asum [traverse_ (sub IntMap.!) xs | xs <- xss]
