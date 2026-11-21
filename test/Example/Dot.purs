module Example.Dot (dot) where

import Prelude

import Data.Array.Indexed (sameLength, zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe)

dot :: Array Number -> Array Number -> Maybe Number
dot xs ys = sameLength xs ys \as bs -> sum (zipWith (*) as bs)
