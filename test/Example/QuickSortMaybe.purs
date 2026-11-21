module Example.QuickSortMaybe (quickSortMaybe) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array.ST (STArray, peek, poke, withArray)
import Data.Array.ST as STArray
import Data.Foldable (for_)

-- | The same QuickSort on a plain `STArray`, where every read is a `Maybe`.
quickSortMaybe :: forall a. Ord a => Array a -> Array a
quickSortMaybe array = ST.run (withArray sortAll array)
  where
  sortAll :: forall h. STArray h a -> ST h Unit
  sortAll arr = do
    n <- STArray.length arr
    sortBetween 0 (n - 1) arr

sortBetween :: forall h a. Ord a => Int -> Int -> STArray h a -> ST h Unit
sortBetween lo hi arr = when (lo < hi) do
  p <- partition lo hi arr
  sortBetween lo (p - 1) arr
  sortBetween (p + 1) hi arr

partition :: forall h a. Ord a => Int -> Int -> STArray h a -> ST h Int
partition lo hi arr = do
  store <- STRef.new lo
  peek hi arr >>= flip for_ \pivot -> tailRecM (scan pivot store) lo
  final <- STRef.read store
  swap final hi
  pure final
  where
  scan pivot store i = do
    peek i arr >>= flip for_ \x -> when (x < pivot) do
      s <- STRef.read store
      swap s i
      void (STRef.write (s + 1) store)
    pure if i + 1 < hi then Loop (i + 1) else Done unit

  swap i j = do
    x <- peek i arr
    y <- peek j arr
    for_ x \x' -> for_ y \y' -> do
      _ <- poke i y' arr
      void (poke j x' arr)
