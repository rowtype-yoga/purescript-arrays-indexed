-- | A mutable array whose length can't change, with indices that are valid
-- | for exactly that array.
-- |
-- | ```purescript
-- | reversed :: forall a. Array a -> Array a
-- | reversed = withArray \arr -> go (first arr) (last arr) arr
-- |   where
-- |   go :: forall s h. Maybe (Index s) -> Maybe (Index s) -> Fixed s h a -> ST h Unit
-- |   go (Just i) (Just j) arr | i < j = do
-- |     swap i j arr
-- |     go (next i arr) (prev j arr) arr
-- |   go _ _ _ = pure unit
-- | ```
-- |
-- | `withFixed` copies an array into a fresh mutable one and brands it with
-- | `s`. You only ever see it as a `Fixed s h a`, which can be read and
-- | written but never grown or shrunk, so every `Index s` stays valid.
module Data.Array.ST.Indexed
  ( Fixed
  , withFixed
  , withArray
  , read
  , write
  , modify
  , swap
  , index
  , indices
  , length
  , first
  , last
  , next
  , prev
  ) where

import Prelude

import Control.Monad.ST (Region, ST)
import Control.Monad.ST as ST
import Data.Array.Indexed (Index, Indexed, withIndexed)
import Data.Array.Indexed as Indexed
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Array.ST.Partial as Partial
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)

-- | A mutable array of fixed length in region `h`, carrying the brand `s`.
data Fixed :: Type -> Region -> Type -> Type
data Fixed s h a = Fixed (Indexed s a) (STArray h a)

type role Fixed nominal nominal nominal

-- | Copies the array, lets you work on the copy, and hands it back with your
-- | result. The brand can't leave the function you pass.
withFixed :: forall h a r. Array a -> (forall s. Fixed s h a -> ST h r) -> ST h (Array a /\ r)
withFixed array use = do
  stArray <- STArray.thaw array
  result <- withIndexed array \original -> use (Fixed original stArray)
  frozen <- STArray.unsafeFreeze stArray
  pure (frozen /\ result)

-- | Runs `withFixed` on its own and keeps only the array.
withArray :: forall a r. (forall s h. Fixed s h a -> ST h r) -> Array a -> Array a
withArray use array = ST.run do
  stArray <- STArray.thaw array
  _ <- withIndexed array \original -> use (Fixed original stArray)
  STArray.unsafeFreeze stArray

read :: forall s h a. Index s -> Fixed s h a -> ST h a
read i (Fixed _ stArray) = unsafePartial (Partial.peek (Indexed.toInt i) stArray)

write :: forall s h a. Index s -> a -> Fixed s h a -> ST h Unit
write i x (Fixed _ stArray) = unsafePartial (Partial.poke (Indexed.toInt i) x stArray)

modify :: forall s h a. Index s -> (a -> a) -> Fixed s h a -> ST h Unit
modify i f arr = do
  x <- read i arr
  write i (f x) arr

-- | Exchanges two elements.
swap :: forall s h a. Index s -> Index s -> Fixed s h a -> ST h Unit
swap i j (Fixed _ stArray) = unsafePartial do
  x <- Partial.peek (Indexed.toInt i) stArray
  y <- Partial.peek (Indexed.toInt j) stArray
  Partial.poke (Indexed.toInt i) y stArray
  Partial.poke (Indexed.toInt j) x stArray

-- | Checks an `Int` once and hands back an index you can use from then on.
index :: forall s h a. Int -> Fixed s h a -> Maybe (Index s)
index i arr = Indexed.index i (shape arr)

-- | Every position, in order.
indices :: forall s h a. Fixed s h a -> Array (Index s)
indices arr = Indexed.indices (shape arr)

length :: forall s h a. Fixed s h a -> Int
length arr = Indexed.length (shape arr)

first :: forall s h a. Fixed s h a -> Maybe (Index s)
first arr = Indexed.first (shape arr)

last :: forall s h a. Fixed s h a -> Maybe (Index s)
last arr = Indexed.last (shape arr)

-- | The position after this one, unless this is the last.
next :: forall s h a. Index s -> Fixed s h a -> Maybe (Index s)
next i arr = Indexed.next i (shape arr)

-- | The position before this one, unless this is the first.
prev :: forall s h a. Index s -> Fixed s h a -> Maybe (Index s)
prev i arr = Indexed.prev i (shape arr)

-- | The original array has the brand and the length of the mutable copy, so
-- | every question about positions goes to it.
shape :: forall s h a. Fixed s h a -> Indexed s a
shape (Fixed original _) = original
