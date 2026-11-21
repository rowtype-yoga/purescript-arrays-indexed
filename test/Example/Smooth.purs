module Example.Smooth (smoothed) where

import Prelude

import Data.Array.Indexed (at, next, toArray, withIndexed)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))

-- | Each value becomes the average of itself and its right neighbour.
smoothed :: Array Number -> Array Number
smoothed signal = withIndexed signal \xs ->
  toArray $ xs # mapWithIndex \i x -> case next i xs of
    Just j -> (x + at j xs) / 2.0
    Nothing -> x
