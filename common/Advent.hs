{-# Language ImportQualifiedPost, OverloadedStrings #-}
module Advent where

import Advent.Coord
import Control.Applicative ((<|>))
import Data.Array.Unboxed qualified as A
import Data.Foldable (toList)
import Data.List
import Data.Ord (comparing)
import Data.Map (Map)
import Data.Map.Strict qualified as SMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
import System.Environment
import Text.Printf

-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getRawInput :: Int {- ^ day number -} -> IO String
getRawInput i =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input%02d.txt" i)
       "-":_ -> getContents
       fn:_  -> readFile fn

inputFileName :: Int -> FilePath
inputFileName = printf "inputs/input%02d.txt"

getInputLines :: Int -> IO [String]
getInputLines i = lines <$> getRawInput i

getInputArray :: Int -> IO (A.UArray Coord Char)
getInputArray i =
  do xs <- getInputLines i
     pure $! A.listArray (C 0 0, C (length xs - 1) (length (head xs) - 1)) (concat xs)

-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc+1 else acc) 0


-- | Return true when the whole list is comprised of equal elements.
--
-- >>> same [1,1,1]
-- True
-- >>> same []
-- True
-- >>> same [1]
-- True
-- >>> same [1,1,2]
-- False
same :: Foldable t => Eq a => t a -> Bool
same xs = all (head (toList xs) ==) xs

-- | Returns a list of ways to select an element from a list without
-- replacement.
--
-- >>> pickOne []
-- []
-- >>> pickOne [1]
-- [(1,[])]
-- >>> pickOne [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pickOne :: [a] -> [(a, [a])]
pickOne xs = [ (x, l++r) | (l,x:r) <- zip (inits xs) (tails xs) ]

-- | Implementation of 'nub' that uses 'Ord' for efficiency.
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise         = x : go (Set.insert x seen) xs


-- | Compute the minimum element of a list or return Nothing if it is empty.
--
-- >>> minimumMaybe []
-- Nothing
-- >>> minimumMaybe [2,1,3]
-- Just 1
minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! minimum xs

-- | Compute the number of occurrences of the elements in a given list.
--
-- >>> cardinality "bababc"
-- fromList [('a',2),('b',3),('c',1)]
cardinality :: Ord a => [a] -> Map a Int
cardinality xs = SMap.fromListWith (+) [ (x,1) | x <- xs ]

-- | Compose a list of functions together
--
-- >>> compose [ (1:), (2:), (3:) ] []
-- [1,2,3]
compose :: [a -> a] -> a -> a
compose = foldr (.) id

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a,b) -> a : chunks n b

-- | Löb's theorem
--
-- <https://github.com/quchen/articles/blob/master/loeb-moeb.md>
-- <https://en.wikipedia.org/wiki/L%C3%B6b%27s_theorem>
löb :: Functor f => f (f a -> a) -> f a
löb = möb fmap

-- | 'löb' generalized over 'fmap'
möb :: (((a -> b) -> b) -> c -> a) -> c -> a
möb f = \x -> let go = f ($ go) x in go

-- | Index an array returning 'Nothing' if the index is out of bounds.
arrIx :: (A.IArray a e, A.Ix i) => a i e -> i -> Maybe e
arrIx a i
  | A.inRange (A.bounds a) i = Just $! a A.! i
  | otherwise = Nothing

-- | Apply a function @n@ times strictly.
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x

uniqueAssignment ::
  (Ord a, Ord b) =>
  [(a, Set b)] {- ^ each @a@ must map to one of the corresponding @b@ -} ->
  [[(a, b)]]    {- ^ assignments of @a@ and @b@ pairs                  -}
uniqueAssignment m =
  case sortBy (comparing (Set.size . snd)) m of
    [] -> [[]]
    (k,vs):rest ->
      [ (k,v) : soln
      | v <- Set.toList vs
      , soln <- uniqueAssignment (fmap (Set.delete v) <$> rest)]
