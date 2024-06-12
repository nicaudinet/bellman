module Test.SimpleProb (tests) where

import Prelude

import Data.Natural (intToNat)
import Data.Tuple (Tuple(..))

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as A 

import SimpleProb (uniform, odds)

testFunctor :: TestSuite
testFunctor = test "functor" do
    let dist = uniform [1, 10]
        res = uniform [2, 11]
    A.equal (map (add 1) dist) res

testApply :: TestSuite
testApply = test "apply" do
    let one = intToNat 1
        two = intToNat 2
        f = uniform [(add 1), (add 2)]
        dist = odds [(Tuple 1 one), (Tuple 10 two)]
        res = odds [(Tuple 2 one), (Tuple 11 two), (Tuple 3 one), (Tuple 12 two)]
    A.equal (apply f dist) res

tests :: TestSuite
tests =
    suite "SimpleProb" do
        testFunctor
        testApply
