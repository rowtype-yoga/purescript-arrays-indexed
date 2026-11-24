-- | Arrays branded with a type `s`, and indices that are valid for exactly
-- | that array.
-- |
-- | ```purescript
-- | smoothed :: Array Number -> Array Number
-- | smoothed signal = withIndexed signal \xs ->
-- |   toArray $ xs # mapWithIndex \i x -> case next i xs of
-- |     Just j -> (x + at j xs) / 2.0
-- |     Nothing -> x
-- | ```
-- |
-- | `withIndexed` gives the array a brand `s` that nothing else shares. An
-- | `Index s` can only come from that array, so `at` needs no `Maybe`.
module Data.Array.Indexed
  ( Indexed
  , Index
  , withIndexed
  , sameLength
  , at
  , index
  , toInt
  , indices
  , length
  , first
  , last
  , next
  , prev
  , zipWith
  , toArray
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FoldableWithIndex as FoldableWithIndex
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.TraversableWithIndex as TraversableWithIndex
import Partial.Unsafe (unsafePartial)

-- | An array carrying the brand `s`.
newtype Indexed :: Type -> Type -> Type
newtype Indexed s a = Indexed (Array a)

type role Indexed nominal representational

-- | A position that is valid in every array carrying the brand `s`.
newtype Index :: Type -> Type
newtype Index s = Index Int

type role Index nominal

derive newtype instance Eq (Index s)
derive newtype instance Ord (Index s)

instance Show (Index s) where
  show (Index i) = "(Index " <> show i <> ")"

derive newtype instance Eq a => Eq (Indexed s a)
derive newtype instance Ord a => Ord (Indexed s a)

instance Show a => Show (Indexed s a) where
  show (Indexed array) = "(Indexed " <> show array <> ")"

derive newtype instance Functor (Indexed s)

instance FunctorWithIndex (Index s) (Indexed s) where
  mapWithIndex f (Indexed array) = Indexed (Array.mapWithIndex (f <<< Index) array)

derive newtype instance Foldable (Indexed s)

instance FoldableWithIndex (Index s) (Indexed s) where
  foldrWithIndex f init (Indexed array) = FoldableWithIndex.foldrWithIndex (f <<< Index) init array
  foldlWithIndex f init (Indexed array) = FoldableWithIndex.foldlWithIndex (f <<< Index) init array
  foldMapWithIndex f (Indexed array) = FoldableWithIndex.foldMapWithIndex (f <<< Index) array

instance Traversable (Indexed s) where
  traverse f (Indexed array) = Indexed <$> traverse f array
  sequence (Indexed array) = Indexed <$> sequence array

instance TraversableWithIndex (Index s) (Indexed s) where
  traverseWithIndex f (Indexed array) = Indexed <$> TraversableWithIndex.traverseWithIndex (f <<< Index) array

-- | Brands an array. The brand can't leave the function you pass.
withIndexed :: forall a r. Array a -> (forall s. Indexed s a -> r) -> r
withIndexed array use = use (Indexed array)

-- | Brands two arrays with the same brand if they have the same length, so
-- | one index works for both.
sameLength :: forall a b r. Array a -> Array b -> (forall s. Indexed s a -> Indexed s b -> r) -> Maybe r
sameLength as bs use
  | Array.length as == Array.length bs = Just (use (Indexed as) (Indexed bs))
  | otherwise = Nothing

-- | The element at a position. Always there.
at :: forall s a. Index s -> Indexed s a -> a
at (Index i) (Indexed array) = unsafePartial (Array.unsafeIndex array i)

-- | Checks an `Int` once and hands back an index you can use from then on.
index :: forall s a. Int -> Indexed s a -> Maybe (Index s)
index i xs
  | i >= 0 && i < length xs = Just (Index i)
  | otherwise = Nothing

-- | Forgets that an index is valid.
toInt :: forall s. Index s -> Int
toInt (Index i) = i

-- | Every position, in order.
indices :: forall s a. Indexed s a -> Array (Index s)
indices (Indexed array) = Array.mapWithIndex (\i _ -> Index i) array

length :: forall s a. Indexed s a -> Int
length (Indexed array) = Array.length array

first :: forall s a. Indexed s a -> Maybe (Index s)
first xs
  | length xs > 0 = Just (Index 0)
  | otherwise = Nothing

last :: forall s a. Indexed s a -> Maybe (Index s)
last xs
  | length xs > 0 = Just (Index (length xs - 1))
  | otherwise = Nothing

-- | The position after this one, unless this is the last.
next :: forall s a. Index s -> Indexed s a -> Maybe (Index s)
next (Index i) xs
  | i + 1 < length xs = Just (Index (i + 1))
  | otherwise = Nothing

-- | The position before this one, unless this is the first.
prev :: forall s a. Index s -> Indexed s a -> Maybe (Index s)
prev (Index i) _
  | i > 0 = Just (Index (i - 1))
  | otherwise = Nothing

-- | Zips two arrays of the same brand. They have the same length, so nothing
-- | is dropped.
zipWith :: forall s a b c. (a -> b -> c) -> Indexed s a -> Indexed s b -> Indexed s c
zipWith f (Indexed as) (Indexed bs) = Indexed (Array.zipWith f as bs)

-- | Forgets the brand.
toArray :: forall s a. Indexed s a -> Array a
toArray (Indexed array) = array
