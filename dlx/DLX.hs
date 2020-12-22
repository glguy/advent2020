{-# Language BlockArguments, ImportQualifiedPost #-}
module DLX (dlx, dlxOpt) where

import Foreign.C
import Foreign.Ptr
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

foreign import ccall dlx_solve ::
  Dlx ->
  FunPtr (CInt -> CInt -> CInt -> IO ()) ->
  FunPtr (IO ()) ->
  FunPtr (IO ()) ->
  FunPtr (CInt -> IO ()) ->
  IO ()

type Wrapper a = a -> IO (FunPtr a)

foreign import ccall "wrapper" mkCover   :: Wrapper (CInt -> CInt -> CInt -> IO ())
foreign import ccall "wrapper" mkUncover :: Wrapper (IO ())
foreign import ccall "wrapper" mkFound   :: Wrapper (IO ())
foreign import ccall "wrapper" mkStuck   :: Wrapper (CInt -> IO ())

push :: IORef [Int] -> CInt -> CInt -> CInt -> IO ()
push ref _ _ r = modifyIORef ref (fromIntegral r:)

pop :: IORef [Int] -> IO ()
pop ref = modifyIORef ref tail

report :: IORef [[Int]] -> IORef [Int] -> IO ()
report solnsRef solnRef =
  do xs <- readIORef solnRef
     modifyIORef solnsRef (xs:)

dlxRaw :: [(Int,Int)] -> [Int] -> IO [[Int]]
dlxRaw ones opts =
  newIORef [] >>= \solnRef ->
  newIORef [] >>= \solnsRef ->
  bracket dlx_new dlx_clear \p ->
  bracket (mkCover   (push solnRef)) freeHaskellFunPtr \cover ->
  bracket (mkUncover (pop  solnRef)) freeHaskellFunPtr \uncover ->
  bracket (mkFound   (report solnsRef solnRef)) freeHaskellFunPtr \found ->
  do for_ ones \(r,c) -> dlx_set p (fromIntegral r) (fromIntegral c)
     for_ opts \r     -> dlx_mark_optional p (fromIntegral r)
     dlx_solve p cover uncover found nullFunPtr
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
