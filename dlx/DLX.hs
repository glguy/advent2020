{-# Language BlockArguments, ImportQualifiedPost #-}
module DLX (DlxLimit(..), dlx, dlxOpt) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Exception
import Data.Foldable
import Data.IORef
import Data.List
import Data.Map qualified as Map
import Data.IntMap qualified as IntMap
import System.IO.Unsafe

data Dlx_
type Dlx = Ptr Dlx_

data DlxLimit = SingleResult | AllResults
  deriving (Eq, Ord, Read, Show)

foreign import ccall unsafe dlx_new :: IO Dlx
foreign import ccall unsafe dlx_clear :: Dlx -> IO ()
foreign import ccall unsafe dlx_rows :: Dlx -> IO CInt
foreign import ccall unsafe dlx_cols :: Dlx -> IO CInt
foreign import ccall unsafe dlx_set :: Dlx -> CInt -> CInt -> IO ()
foreign import ccall unsafe dlx_mark_optional :: Dlx -> CInt -> IO ()
foreign import ccall unsafe dlx_remove_row :: Dlx -> CInt -> IO ()
foreign import ccall unsafe dlx_pick_row :: Dlx -> CInt -> IO CInt

type Wrapper a = a -> IO (FunPtr a)
foreign import ccall "wrapper" mkCB :: Wrapper (Ptr CInt -> CInt -> IO ())
foreign import ccall dlx_forall :: Dlx -> CInt -> FunPtr (Ptr CInt -> CInt -> IO ()) -> IO ()

report :: IORef [[Int]] -> Ptr CInt -> CInt -> IO ()
report solnsRef ptr n =
  do xs <- peekArray (fromIntegral n) ptr
     modifyIORef solnsRef (map fromIntegral xs:)

dlxRaw :: DlxLimit -> [(Int,Int)] -> [Int] -> [[Int]]
dlxRaw limit ones opts =
  unsafeDupablePerformIO $
  newIORef [] >>= \solnsRef ->
  bracket dlx_new dlx_clear \p ->
  do for_ ones \(r,c) -> dlx_set p (fromIntegral r) (fromIntegral c)
     for_ opts \r     -> dlx_mark_optional p (fromIntegral r)
     let single_ = case limit of SingleResult -> 1; AllResults -> 0
     bracket (mkCB (report solnsRef)) freeHaskellFunPtr (dlx_forall p single_)
     readIORef solnsRef

dlx :: (Ord a, Ord b) => [(a,b)] -> [[a]]
dlx input = dlxOpt SingleResult input (const False)

dlxOpt :: (Ord a, Ord b) => DlxLimit -> [(a,b)] -> (b -> Bool) -> [[a]]
dlxOpt limit input opt =
  [[i2a IntMap.! a | a <- answer] | answer <- answers]
  where
    as = nub (sort (map fst input))
    bs = nub (sort (map snd input))
    a2i = Map.fromList (zip as [0..])
    b2i = Map.fromList (zip bs [0..])
    i2a = IntMap.fromList (zip [0..] as)

    answers = dlxRaw limit
                  [(a2i Map.! a, b2i Map.! b) | (a,b) <- input]
                  [i | (b,i) <- zip bs [0..], opt b]
