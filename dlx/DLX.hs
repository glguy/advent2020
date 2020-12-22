{-# Language BlockArguments, ImportQualifiedPost #-}
module DLX (dlx, dlxOpt) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Exception
import Data.Foldable
import Data.IORef
import Data.List
import Data.Map qualified as Map
import Data.IntMap qualified as IntMap

data Dlx_
type Dlx = Ptr Dlx_

foreign import ccall unsafe dlx_new :: IO Dlx
foreign import ccall unsafe dlx_clear :: Dlx -> IO ()
foreign import ccall unsafe dlx_rows :: Dlx -> IO CInt
foreign import ccall unsafe dlx_cols :: Dlx -> IO CInt
foreign import ccall unsafe dlx_set :: Dlx -> CInt -> CInt -> IO ()
foreign import ccall unsafe dlx_mark_optional :: Dlx -> CInt -> IO ()
foreign import ccall unsafe dlx_remove_row :: Dlx -> CInt -> IO ()
foreign import ccall unsafe dlx_pick_row :: Dlx -> CInt -> IO CInt
foreign import ccall dlx_forall :: Dlx -> FunPtr (Ptr CInt -> CInt -> IO ()) -> IO ()

foreign import ccall dlx_solve ::
  Dlx ->
  Ptr () ->
  FunPtr (Ptr () -> CInt -> CInt -> CInt -> IO ()) ->
  FunPtr (Ptr () -> IO ()) ->
  FunPtr (Ptr () -> IO ()) ->
  FunPtr (Ptr () -> CInt -> IO ()) ->
  IO ()

type Wrapper a = a -> IO (FunPtr a)
foreign import ccall "wrapper" mkCB :: Wrapper (Ptr CInt -> CInt -> IO ())

report :: IORef [[Int]] -> Ptr CInt -> CInt -> IO ()
report solnsRef ptr n =
  do xs <- peekArray (fromIntegral n) ptr
     modifyIORef solnsRef (map fromIntegral xs:)

dlxRaw :: [(Int,Int)] -> [Int] -> IO [[Int]]
dlxRaw ones opts =
  newIORef [] >>= \solnsRef ->
  bracket dlx_new dlx_clear \p ->
  do for_ ones \(r,c) -> dlx_set p (fromIntegral r) (fromIntegral c)
     for_ opts \r     -> dlx_mark_optional p (fromIntegral r)
     bracket (mkCB (report solnsRef)) freeHaskellFunPtr (dlx_forall p)
     readIORef solnsRef

dlx :: (Ord a, Ord b) => [(a,b)] -> IO [[a]]
dlx input = dlxOpt input (const False)

dlxOpt :: (Ord a, Ord b) => [(a,b)] -> (b -> Bool) -> IO [[a]]
dlxOpt input opt =
  do let as = nub (sort (map fst input))
         bs = nub (sort (map snd input))
         a2i = Map.fromList (zip as [0..])
         b2i = Map.fromList (zip bs [0..])
         i2a = IntMap.fromList (zip [0..] as)

     answers <- dlxRaw [(a2i Map.! a, b2i Map.! b) | (a,b) <- input]
                       [i | (b,i) <- zip bs [0..], opt b]
     pure [[i2a IntMap.! a | a <- answer] | answer <- answers]