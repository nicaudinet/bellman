module Test.RoomWalk (tests) where

import Data.Array.NonEmpty (length)
import Data.Int (pow)

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as A

import RoomWalk (possiblePolicies)

testPossiblePolicies :: TestSuite
testPossiblePolicies = test "possiblePolicies" do
    A.equal (length possiblePolicies) (pow 4 6)

tests :: TestSuite
tests =
    suite "RandomWalk" do
        testPossiblePolicies
