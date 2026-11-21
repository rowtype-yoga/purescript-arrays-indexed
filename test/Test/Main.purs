module Test.Main (main) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (error)
import Test.Data.Array.Indexed as Indexed
import Test.Data.Array.ST.Indexed as STIndexed
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, evalSpecT)
import Test.Spec.Summary (successful)

-- | Runs on every backend: fails the program when a test fails instead of
-- | relying on Node to set the exit code.
main :: Effect Unit
main = launchAff_ do
  results <- un Identity (evalSpecT defaultConfig [ consoleReporter ] spec)
  unless (successful results) (throwError (error "Some tests failed"))
  where
  spec = do
    Indexed.spec
    STIndexed.spec
