module Example.QuickSort (quickSort) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST.Ref as STRef
import Data.Array.Indexed (Index)
import Data.Array.ST.Indexed (Fixed, first, last, next, prev, read, swap, withArray)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

quickSort :: forall a. Ord a => Array a -> Array a
quickSort = withArray \arr -> case first arr, last arr of
  Just lo, Just hi -> sortBetween lo hi arr
  _, _ -> pure unit

sortBetween :: forall s h a. Ord a => Index s -> Index s -> Fixed s h a -> ST h Unit
sortBetween lo hi arr = when (lo < hi) do
  p <- partition lo hi arr
  for_ (prev p arr) \left -> sortBetween lo left arr
  for_ (next p arr) \right -> sortBetween right hi arr

-- | Moves everything smaller than the pivot (the element at `hi`) to the
-- | left of it and returns where the pivot ends up.
partition :: forall s h a. Ord a => Index s -> Index s -> Fixed s h a -> ST h (Index s)
partition lo hi arr = do
  pivot <- read hi arr
  store <- STRef.new lo
  tailRecM (scan pivot store) lo
  final <- STRef.read store
  swap final hi arr
  pure final
  where
  scan pivot store i = do
    x <- read i arr
    when (x < pivot) (moveLeft store i)
    pure case next i arr of
      Just i' | i' < hi -> Loop i'
      _ -> Done unit

  moveLeft store i = do
    s <- STRef.read store
    swap s i arr
    for_ (next s arr) \s' -> STRef.write s' store
