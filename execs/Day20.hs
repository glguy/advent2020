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

import Advent (getRawInput, pickOne)
import Advent.Coord (Coord(C), turnRight, invert, addCoord, coordRow, above, left)
import Advent.InputParser (format)
import Control.Monad (guard)
import Data.Foldable (foldl', for_)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Ix

type Picture = [Coord]

-- <https://www.youtube.com/watch?v=EIyixC9NsLI>
snek :: Picture
snek = toPicture
  ["                  # "
  ,"#    ##    ##    ###"
  ," #  #  #  #  #  #   "]

-- the final image is a 12x12 arrangement of tiles
sz :: Int
sz = 12

-- | Rotate an image 90 degrees clockwise
rotate :: Picture -> Picture
rotate xs = map (addCoord (C 0 n) . turnRight) xs
  where n = maximum (map coordRow xs)

-- | Generate all 8 rotations and flips of a picture
reorient :: Picture -> [Picture]
reorient xs =
  take 4 (iterate rotate xs) ++
  take 4 (iterate rotate (map invert xs))

toPicture :: [String] -> Picture
toPicture rs = [C y x | (y,r) <- zip [0..] rs, (x,'#') <- zip [0..] r]

main :: IO ()
main =
  do inp <- map (fmap toPicture) <$> [format|20 (Tile %u:%n(%s%n)*%n)*|]

     -- arrange all the tiles
     let tileLocations = range (C 0 0, C (sz-1) (sz-1))
     let aligned = head (stitch Map.empty inp tileLocations)
     print (product [fst (aligned Map.! C y x) | y <- [0,sz-1], x <- [0,sz-1]])

     -- assemble the complete image while removing borders
     let pic = Set.fromList
                [ C (8*yy+y-1) (8*xx+x-1)
                | (C yy xx, (_,cell)) <- Map.toList aligned
                , C y x <- cell
                , y /= 9, x /= 9, y /= 0, x /= 0]

     -- cut all the snakes out of the picture
     let p2 = foldl' cut pic
              [map (addCoord (C dy dx)) s -- translate the tile
              | dx <- [0..8*sz]
              , dy <- [0..8*sz]
              , s  <- reorient snek
              ]

     print (Set.size p2)

-- | If a picture is contained in the coordinate set, delete it
cut :: Set Coord -> Picture -> Set Coord
cut m s
  | s' `Set.isSubsetOf` m = m Set.\\ s'
  | otherwise             = m
  where
    s' = Set.fromList s

stitch :: Map Coord (Int,[Coord]) -> [(Int,[Coord])] -> [Coord] -> [Map Coord (Int, [Coord])]
stitch m _ [] = [m]
stitch m avail (c:cs) =
  do -- pick one of the remaining tiles and orient it
     ((i,cell_), avail') <- pickOne avail
     cell <- reorient cell_

     -- match the tile above if there is one
     for_ (Map.lookup (above c) m) \(_,l) ->
       guard (sort [x | C 0 x <- cell] == sort [x | C 9 x <- l])

     -- match the tile to the left if there is one
     for_ (Map.lookup (left c) m) \(_,l) ->
       guard (sort [y | C y 0 <- cell] == sort [y | C y 9 <- l])

     stitch (Map.insert c (i,cell) m) avail' cs
