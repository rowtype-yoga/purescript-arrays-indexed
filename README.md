# purescript-arrays-indexed

Array indices that are always valid, so reading an element never hands you a `Maybe`.

```purescript
import Data.Array.Indexed (at, next, toArray, withIndexed)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))

-- Each value becomes the average of itself and its right neighbour.
smoothed :: Array Number -> Array Number
smoothed signal = withIndexed signal \xs ->
  toArray $ xs # mapWithIndex \i x -> case next i xs of
    Just j -> (x + at j xs) / 2.0
    Nothing -> x
```

`i` came from `mapWithIndex` and `j` came from `next`, so both are positions in `xs`. `at` doesn't
check anything and doesn't return a `Maybe`. The only `Maybe` left is the one you actually care
about: whether there's a right neighbour.

## Installation

```
spago install arrays-indexed
```

## Why

With `Data.Array.index` every lookup is a `Maybe`, even when you just got the index from the same
array. You end up writing `fromMaybe 0.0` or `unsafePartial` all over the place, and neither tells
the compiler what you know.

`withIndexed` gives your array a type `s` that no other array has. An `Index s` can only be made
for that array, so it's always in range.

## Reading: `Data.Array.Indexed`

| | |
| --- | --- |
| `withIndexed :: Array a -> (forall s. Indexed s a -> r) -> r` | brand an array |
| `at :: Index s -> Indexed s a -> a` | read, no `Maybe` |
| `index :: Int -> Indexed s a -> Maybe (Index s)` | check an `Int` once |
| `indices :: Indexed s a -> Array (Index s)` | every position |
| `first`, `last :: Indexed s a -> Maybe (Index s)` | the ends, if there are any |
| `next`, `prev :: Index s -> Indexed s a -> Maybe (Index s)` | the neighbours |
| `length :: Indexed s a -> Int` | |
| `toInt :: Index s -> Int` | forget that it's valid |
| `toArray :: Indexed s a -> Array a` | forget the brand |
| `sameLength :: Array a -> Array b -> (forall s. Indexed s a -> Indexed s b -> r) -> Maybe r` | one brand for two arrays |
| `zipWith :: (a -> b -> c) -> Indexed s a -> Indexed s b -> Indexed s c` | zip, nothing dropped |

`Indexed s` is a `Functor`, `Foldable` and `Traversable`, and the `WithIndex` versions of those
use `Index s`. So `mapWithIndex`, `foldlWithIndex`, `traverseWithIndex` and `forWithIndex_` all
hand you indices you can use right away.

Got several arrays that should line up? Check once with `sameLength` and use the same index for all
of them:

```purescript
dot :: Array Number -> Array Number -> Maybe Number
dot xs ys = sameLength xs ys \as bs -> sum (zipWith (*) as bs)
```

## Writing: `Data.Array.ST.Indexed`

For in-place algorithms there's `Fixed s h a`, a mutable array that can't grow or shrink. Here's
QuickSort:

```purescript
import Data.Array.ST.Indexed (Fixed, first, last, next, prev, read, swap, withArray)

quickSort :: forall a. Ord a => Array a -> Array a
quickSort = withArray \arr -> case first arr, last arr of
  Just lo, Just hi -> sortBetween lo hi arr
  _, _ -> pure unit

sortBetween :: forall s h a. Ord a => Index s -> Index s -> Fixed s h a -> ST h Unit
sortBetween lo hi arr = when (lo < hi) do
  p <- partition lo hi arr
  for_ (prev p arr) \left -> sortBetween lo left arr
  for_ (next p arr) \right -> sortBetween right hi arr
```

The whole thing, including `partition`, is in [`test/Example/QuickSort.purs`](test/Example/QuickSort.purs).

| | |
| --- | --- |
| `withFixed :: Array a -> (forall s. Fixed s h a -> ST h r) -> ST h (Array a /\ r)` | work on a copy inside `ST h` |
| `withArray :: (forall s h. Fixed s h a -> ST h r) -> Array a -> Array a` | the same, as a pure function |
| `read :: Index s -> Fixed s h a -> ST h a` | |
| `write :: Index s -> a -> Fixed s h a -> ST h Unit` | |
| `modify :: Index s -> (a -> a) -> Fixed s h a -> ST h Unit` | |
| `swap :: Index s -> Index s -> Fixed s h a -> ST h Unit` | |
| `index`, `indices`, `length`, `first`, `last`, `next`, `prev` | as above |

`withFixed` takes an `Array`, not an `STArray`. If it took your `STArray` you could still `pop`
from it while holding indices into it.

## How it's safe

The brand only exists inside the function you pass to `withIndexed`, and none of the ways to get an
`Index s` let you make one that's out of range. These all fail to compile:

```purescript
-- The index can't leave.
escaped = withIndexed [ 1, 2, 3 ] \xs -> first xs
-- The type variable s ... has escaped its scope

-- An index for one array doesn't work on another.
mixed = withIndexed [ 1, 2, 3 ] \long -> withIndexed [ 1 ] \short ->
  map (\i -> at i short) (last long)
-- Could not match type s2 with type s1

-- No rebranding with coerce.
rebrand :: forall s t. Index s -> Index t
rebrand = coerce
-- Could not match type s0 with type t1

-- No making one up.
forged :: forall s. Index s
forged = wrap 7
-- No type class instance was found for Newtype (Index s0) Int
```

This is the "lightweight static capabilities" trick from Kiselyov and Shan, also known as
"Ghosts of Departed Proofs".

## On Go

There's no FFI, so it works on every backend. With [Iris](https://github.com/i-am-the-slime/purescript-analyzer/tree/iris-go)'s
Go backend `at`, `read` and `write` are plain `xs[i]`, and the smoothing example above becomes a
single loop:

```go
for slot, x := range signal {
	x2 := int32(slot)
	i := x2 + 1
	var result2 float64
	if i >= 0 && i < int32(len(signal)) {
		result2 = (x + signal[i]) / 2.0
	} else {
		result2 = x
	}
	mapped6[slot] = result2
}
```

The `if` is `next`. That's the check you asked for.
