module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)

import Test.SimpleProb as SimpleProb

main :: Effect Unit
main = do
  runTest SimpleProb.tests
