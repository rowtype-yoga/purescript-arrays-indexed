module Bench.Main (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (and)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (now)
import Example.QuickSort (quickSort)
import Example.QuickSortMaybe (quickSortMaybe)

main :: Effect Unit
main = do
  time "withFixed (no Maybe)" quickSort
  time "STArray peek/poke (Maybe)" quickSortMaybe
  where
  time label sort = do
    start <- now
    let sorted = sort input
    unless (isSorted sorted) (log (label <> " did not sort"))
    end <- now
    log (label <> ": " <> show (millis end - millis start) <> " ms")

  isSorted xs = and (Array.zipWith (<=) xs (Array.drop 1 xs))

  millis instant = un Milliseconds (unInstant instant)

  input = Array.range 1 1_000_000 <#> \k -> ((k `mod` 100_000) * 7_919 + k) `mod` 999_983
