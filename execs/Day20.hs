{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-# Options_GHC -w #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/20>

-}
module Main (main) where

import Advent             (cardinality, pickOne)
import Advent.Coord
import Advent.Format (format)
import Control.Monad      (guard)
import Data.Foldable      (foldl', for_)
import Data.Ix            (range)
import Data.List          (mapAccumL, sort)
import Data.Map           (Map)
import Data.Map qualified as Map
import Data.Set           (Set)
import Data.Set qualified as Set
import Data.Array qualified as A

type Picture = [Coord]

-- <https://www.youtube.com/watch?v=EIyixC9NsLI>
snek :: Picture
snek =
  toPicture
    ["                  # "
    ,"#    ##    ##    ###"
    ," #  #  #  #  #  #   "]

-- the final image is a 12x12 arrangement of tiles
sz :: Int
sz = 12

-- | Rotate an image 90 degrees clockwise
rotate :: Picture -> Picture
rotate xs = map (addCoord (C 0 n) . turnRight) xs
  where
    n = maximum (map coordRow xs)

-- | Generate all 8 rotations and flips of a picture
reorient :: Picture -> [Picture]
reorient xs =
  do xs' <- take 4 (iterate rotate xs)
     [xs', map invert xs']

toPicture :: [String] -> Picture
toPicture rs = [C y x | (y,r) <- zip [0..] rs, (x,'#') <- zip [0..] r]

-- |
-- >>> :main
-- 8581320593371
-- 2031
main :: IO ()
main =
  do inp <- map (fmap toPicture) <$> [format|20 (Tile %u:%n(%s%n)*%n)*|]

     -- "Tiles at the edge of the image also have this border,
     -- but the outermost edges won't line up with any other tiles."
     let edges
           = Map.keysSet
           $ Map.filter (1==)
           $ cardinality [code | (_,pic) <- inp, code <- edgeCodes pic]

     -- arrange all the tiles
     let aligned = place edges inp

     -- print the product of the corner tile IDs
     print $ product [fst (aligned A.! C y x) | y <- [0, sz-1], x <- [0, sz-1]]

     -- assemble the complete image while removing borders
     let pic =
           Set.fromList
             [ C (8*yy+y-1) (8*xx+x-1)
             | (C yy xx, (_,cell)) <- A.assocs aligned
             , c@(C y x) <- cell
             , y /= 0, x /= 0, y /= 9, x /= 9 -- remove edges
             ]

     -- cut all the snakes out of the picture
     print $ Set.size
           $ foldl' cut pic
               [ Set.fromList (addCoord d <$> s) -- translate the tile
               | d <- range (C 0 0, C (8*sz) (8*sz))
               , s <- reorient snek
               ]

-- | Remove the needle from the haystack if the whole needle is found
-- in the haystack.
cut ::
  Ord a =>
  Set a {- ^ haystack -} ->
  Set a {- ^ needle   -} ->
  Set a
cut m s
  | s `Set.isSubsetOf` m = m Set.\\ s
  | otherwise            = m

place ::
  Set [Int] {- ^ edges -} ->
  [(Int,Picture)] {- ^ tiles -} ->
  A.Array Coord (Int, Picture) {- ^ arranged image -}
place edges tiles = board
  where
    bnds = (C 0 0, C (sz-1) (sz-1))
    board = A.listArray bnds (snd (mapAccumL pickTile tiles (range bnds)))
    pickTile avail c = head
      [ (avail', (tileId, cell))
      | ((tileId,cell_), avail') <- pickOne avail
      , cell                     <- reorient cell_
      , if coordRow c == 0
          then normalize (topEdge cell) `Set.member` edges
          else topEdge cell == bottomEdge (snd (board A.! above c))
      , if coordCol c == 0
          then normalize (leftEdge cell) `Set.member` edges
          else leftEdge cell == rightEdge (snd (board A.! left c))
      ]

-- | Extract all the normalized edge codes for a tile
edgeCodes :: Picture -> [[Int]]
edgeCodes xs = [normalize (f xs) | f <- [topEdge, leftEdge, bottomEdge, rightEdge]]

topEdge, leftEdge, bottomEdge, rightEdge :: [Coord] -> [Int]
topEdge    xs = sort [x | C 0 x <- xs]
leftEdge   xs = sort [y | C y 0 <- xs]
bottomEdge xs = sort [x | C 9 x <- xs]
rightEdge  xs = sort [y | C y 9 <- xs]

-- | Normalize a code so that it can be identified even when flipped over
normalize :: [Int] -> [Int]
normalize x = min x (reverse (map (9-) x))
