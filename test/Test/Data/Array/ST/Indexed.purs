module Test.Data.Array.ST.Indexed (spec) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array.Indexed (Index, toInt)
import Data.Array.ST.Indexed (Fixed, first, index, indices, last, length, modify, next, prev, read, replicateLike, swap, withArray, withFixed, write)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Example.QuickSort (quickSort)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Data.Array.ST.Indexed" do
  describe "withFixed" do
    it "hands back the changed copy and your result" do
      ST.run (withFixed [ 1, 2, 3 ] \arr -> for_ (first arr) (\i -> write i 10 arr) $> length arr)
        `shouldEqual` ([ 10, 2, 3 ] /\ 3)
    it "leaves the original array alone" do
      let input = [ 1, 2, 3 ]
      let changed = withArray (\arr -> for_ (indices arr) \i -> write i 0 arr) input
      (input /\ changed) `shouldEqual` ([ 1, 2, 3 ] /\ [ 0, 0, 0 ])
    it "works inside a bigger ST computation" do
      ST.run do
        total <- STRef.new 0
        _ <- withFixed [ 1, 2, 3 ] \arr -> for_ (indices arr) \i -> do
          x <- read i arr
          STRef.modify (_ + x) total
        STRef.read total
        `shouldEqual` 6

  describe "replicateLike" do
    it "makes an array of the same length whose indices are the first's" do
      ST.run (withFixed [ "a", "b", "c" ] \letters -> do
        counts <- replicateLike 0 letters
        for_ (indices letters) \i -> read i letters >>= \letter -> write i (if letter == "b" then 2 else 1) counts
        for_ (last letters) \i -> modify i (_ + 10) counts
        for (indices letters) \i -> read i counts)
        `shouldEqual` ([ "a", "b", "c" ] /\ [ 1, 2, 11 ])

  describe "read and write" do
    it "write replaces the element read returns" do
      withArray (\arr -> for_ (index 1 arr) \i -> write i 20 arr) [ 1, 2, 3 ] `shouldEqual` [ 1, 20, 3 ]

  describe "modify" do
    it "applies a function to every element" do
      withArray (\arr -> for_ (indices arr) \i -> modify i (_ * 2) arr) [ 1, 2, 3 ] `shouldEqual` [ 2, 4, 6 ]

  describe "swap" do
    it "exchanges two elements" do
      withArray (\arr -> for_ (first arr) \i -> for_ (last arr) \j -> swap i j arr) [ 1, 2, 3 ] `shouldEqual` [ 3, 2, 1 ]

  describe "index" do
    it "checks the position once" do
      ST.run (withFixed [ 1, 2, 3 ] \arr -> pure (map toInt (index 2 arr) /\ map toInt (index 3 arr) /\ map toInt (index (-1) arr)))
        `shouldEqual` ([ 1, 2, 3 ] /\ Just 2 /\ Nothing /\ Nothing)

  describe "next and prev" do
    it "walk from both ends to the middle" do
      reversed [ 1, 2, 3, 4, 5 ] `shouldEqual` [ 5, 4, 3, 2, 1 ]
    it "handle empty arrays" do
      reversed ([] :: Array Int) `shouldEqual` []

  describe "quickSort" do
    it "sorts" do
      quickSort [ 5, 3, 9, 1, 1, 7, 2, 8, 0, 6 ] `shouldEqual` [ 0, 1, 1, 2, 3, 5, 6, 7, 8, 9 ]
    it "sorts sorted and empty arrays" do
      (quickSort [ 1, 2, 3 ] /\ quickSort ([] :: Array Int)) `shouldEqual` ([ 1, 2, 3 ] /\ [])
  where
  reversed :: forall a. Array a -> Array a
  reversed = withArray \arr -> go (first arr) (last arr) arr
    where
    go :: forall s h. Maybe (Index s) -> Maybe (Index s) -> Fixed s h a -> ST h Unit
    go (Just i) (Just j) arr | i < j = do
      swap i j arr
      go (next i arr) (prev j arr) arr
    go _ _ _ = pure unit
