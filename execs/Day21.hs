{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/21>

-}
module Main (main) where

import Advent (count)
import Advent.Format (format)
import Data.List (intercalate, minimumBy, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main =
  do inp <- [format|21 (%s&  %(contains %s&(, )%)%n)*|]
     let solns    = solve (toConstraints inp)
         badFoods = Set.fromList [food | soln <- solns, (_,food) <- soln]

     print (count (`Set.notMember` badFoods) (concatMap fst inp))
     putStrLn (intercalate "," (map snd (sort (head solns))))

toConstraints :: (Ord a, Ord b) => [([a],[b])] -> Map b (Set a)
toConstraints inp =
  Map.fromListWith Set.intersection
    [ (allergen, Set.fromList foods)
    | (foods, allergens) <- inp
    , allergen <- allergens
    ]

solve ::
  (Ord a, Ord b) =>
  Map a (Set b) {- ^ each @a@ must map to one of the corresponding @b@ -} ->
  [[(a, b)]]    {- ^ assignments of @a@ and @b@ pairs                  -}
solve m =
  case minimumBy (comparing (Set.size . snd)) (Map.assocs m) of
    _ | Map.null m -> [[]]
    (allergen, foods) ->
      [ (allergen, food) : soln
      | food <- Set.toList foods
      , soln <- solve (Set.delete food <$> Map.delete allergen m)
      ]
