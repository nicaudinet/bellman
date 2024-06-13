module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)

import Test.SimpleProb as SimpleProb
import Test.RoomWalk as RoomWalk

main :: Effect Unit
main = do
  runTest SimpleProb.tests
  runTest RoomWalk.tests
