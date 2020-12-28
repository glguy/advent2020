{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/20>

-}
module Main (main) where

import Advent (count, same)
import Advent.Coord
import Advent.Format (format)
import Data.Array qualified as A
import Data.List (sort)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

type Picture = [Coord]

-- <https://www.youtube.com/watch?v=EIyixC9NsLI>
snek :: Picture
snek =
  toPicture
    ["                  # "
    ,"#    ##    ##    ###"
    ," #  #  #  #  #  #   "]

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

-- | Characterize image orientations by their left edge
edgeMap :: [(Int,Picture)] -> Map [Int] [(Int,Picture)]
edgeMap xs =
  Map.fromListWith (++)
    [(leftEdge pic', [(i,pic')]) | (i, pic) <- xs, pic' <- reorient pic]

-- |
-- >>> :main
-- 8581320593371
-- 2031
main :: IO ()
main =
  do inp <- map (fmap toPicture) <$> [format|20 (Tile %u:%n(%s%n)*%n)*|]

     -- group available pictures by their left-edge code
     let em = edgeMap inp

     -- pick a tile with a unique left and top edge to be the first corner
     let corner = head
                   [ x | x:xs <- Map.elems em
                       , let sameCodes = same . map fst -- edge codes that only match themselves
                       , sameCodes (x:xs)
                       , sameCodes (em Map.! topEdge (snd x))]

     -- arrange all the tiles
     let image = place em corner

     -- print the product of the corner tile IDs
     print $ product [fst (image A.! C y x) | y <- [0, 11], x <- [0, 11]]

     -- assemble the complete image while removing borders
     let pixels =
           Set.fromList
             [ C (8*yy+y-1) (8*xx+x-1)
             | (C yy xx, (_,cell)) <- A.assocs image
             , C y x <- cell
             , y /= 0, x /= 0, y /= 9, x /= 9 -- remove edges
             ]

     -- count occurrences of the snake in the pixel set
     let n = count (`Set.isSubsetOf` pixels)
             [ Set.mapMonotonic (addCoord d) s -- translate the tile
             | s <- Set.fromList <$> reorient snek
             , d <- A.range (C 0 0, C (8*12-1) (8*12-1))
             ]

     -- cut all the snakes out of the picture
     print (Set.size pixels - n * length snek)

place ::
  Map [Int] [(Int, Picture)] ->
  (Int, Picture) ->
  A.Array Coord (Int, Picture) {- ^ arranged image -}
place em start = board
  where
    bnds = (C 0 0, C 11 11)
    board = A.listArray bnds (pickTile <$> A.range bnds)

    pickTile c
      | c == origin = start

      | coordRow c == 0 =
         head [ (tileId, pic)
              | let (nbId, nbPic) = board A.! left c
              , (tileId, pic) <- em Map.! rightEdge nbPic
              , tileId /= nbId ]

      | otherwise =
         head [ (tileId, rotate pic)
              | let (nbId, nbPic) = board A.! above c
              , (tileId, pic) <- em Map.! flipCode (bottomEdge nbPic)
              , tileId /= nbId ]

topEdge, leftEdge, bottomEdge, rightEdge :: [Coord] -> [Int]
topEdge    xs = sort [x | C 0 x <- xs]
leftEdge   xs = sort [y | C y 0 <- xs]
bottomEdge xs = sort [x | C 9 x <- xs]
rightEdge  xs = sort [y | C y 9 <- xs]

flipCode :: [Int] -> [Int]
flipCode = reverse . map (9-)
