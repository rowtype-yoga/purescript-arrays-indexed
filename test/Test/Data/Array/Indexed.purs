module Test.Data.Array.Indexed (spec) where

import Prelude

import Data.Array.Indexed (at, first, index, indices, last, length, next, prev, sameLength, toArray, toInt, withIndexed, zipWith)
import Data.Foldable (sum)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Traversable (traverse)
import Example.Dot (dot)
import Example.Smooth (smoothed)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Data.Array.Indexed" do
  describe "at" do
    it "reads an element at an index the array handed out" do
      withIndexed letters (\xs -> map (\i -> at i xs) (indices xs)) `shouldEqual` letters

  describe "index" do
    it "accepts positions inside the array" do
      withIndexed letters (\xs -> map (\i -> at i xs) (index 1 xs)) `shouldEqual` Just "b"
    it "rejects negative positions" do
      withIndexed letters (\xs -> map toInt (index (-1) xs)) `shouldEqual` Nothing
    it "rejects positions past the end" do
      withIndexed letters (\xs -> map toInt (index 3 xs)) `shouldEqual` Nothing

  describe "indices" do
    it "lists every position in order" do
      withIndexed letters (\xs -> map toInt (indices xs)) `shouldEqual` [ 0, 1, 2 ]
    it "is empty for an empty array" do
      withIndexed noLetters (\xs -> map toInt (indices xs)) `shouldEqual` []

  describe "length" do
    it "is the length of the array" do
      withIndexed letters length `shouldEqual` 3

  describe "first and last" do
    it "point at the ends" do
      withIndexed letters (\xs -> map (\i -> at i xs) (first xs) /\ map (\i -> at i xs) (last xs))
        `shouldEqual` (Just "a" /\ Just "c")
    it "are missing for an empty array" do
      withIndexed noLetters (\xs -> map toInt (first xs) /\ map toInt (last xs))
        `shouldEqual` (Nothing /\ Nothing)

  describe "next and prev" do
    it "step between neighbours" do
      withIndexed letters (\xs -> map toInt (index 1 xs >>= \i -> next i xs) /\ map toInt (index 1 xs >>= \i -> prev i xs))
        `shouldEqual` (Just 2 /\ Just 0)
    it "stop at the ends" do
      withIndexed letters (\xs -> map toInt (last xs >>= \i -> next i xs) /\ map toInt (first xs >>= \i -> prev i xs))
        `shouldEqual` (Nothing /\ Nothing)

  describe "instances" do
    it "mapWithIndex hands out indices of the same array" do
      withIndexed letters (\xs -> toArray (mapWithIndex (\i x -> x <> at i xs) xs)) `shouldEqual` [ "aa", "bb", "cc" ]
    it "foldlWithIndex and foldrWithIndex visit every index" do
      withIndexed [ 10, 20, 30 ] (\xs -> foldlWithIndex (\i acc x -> acc + toInt i * x) 0 xs /\ foldrWithIndex (\i x acc -> acc + toInt i * x) 0 xs)
        `shouldEqual` (80 /\ 80)
    it "traverseWithIndex keeps the brand" do
      withIndexed [ 1, 2, 3 ] (\xs -> map toArray (traverseWithIndex (\i x -> if x > 0 then Just (x + toInt i) else Nothing) xs))
        `shouldEqual` Just [ 1, 3, 5 ]
    it "traverse stops on failure" do
      withIndexed [ 1, 0, 3 ] (\xs -> map toArray (traverse (\x -> if x > 0 then Just x else Nothing) xs))
        `shouldEqual` Nothing
    it "Foldable sums the elements" do
      withIndexed [ 1, 2, 3 ] sum `shouldEqual` 6
    it "Show prints an index" do
      withIndexed letters (\xs -> map show (last xs)) `shouldEqual` Just "(Index 2)"

  describe "sameLength" do
    it "brands arrays of equal length together" do
      sameLength [ 1, 2, 3 ] [ 10, 20, 30 ] (\xs ys -> map (\i -> at i xs + at i ys) (indices xs))
        `shouldEqual` Just [ 11, 22, 33 ]
    it "refuses arrays of different length" do
      sameLength [ 1, 2, 3 ] [ 10, 20 ] (\xs _ -> length xs) `shouldEqual` Nothing

  describe "zipWith" do
    it "keeps every element of both arrays" do
      sameLength [ 1, 2, 3 ] [ 10, 20, 30 ] (\xs ys -> toArray (zipWith (+) xs ys))
        `shouldEqual` Just [ 11, 22, 33 ]

  describe "dot" do
    it "multiplies two vectors after one length check" do
      (dot [ 1.0, 2.0, 3.0 ] [ 4.0, 5.0, 6.0 ] /\ dot [ 1.0 ] []) `shouldEqual` (Just 32.0 /\ Nothing)

  describe "smoothing" do
    it "averages each value with its right neighbour" do
      smoothed [ 1.0, 3.0, 5.0, 7.0, 9.0 ] `shouldEqual` [ 2.0, 4.0, 6.0, 8.0, 9.0 ]
  where
  letters = [ "a", "b", "c" ]
  noLetters = [] :: Array String
