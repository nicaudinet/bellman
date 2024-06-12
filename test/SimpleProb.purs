module Test.SimpleProb (tests) where

import Prelude

import Data.Natural (Natural, intToNat)
import Data.Tuple (Tuple(..))

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as A

import SimpleProb (SP, uniform, odds, condense, sort)

two :: Natural
two = intToNat 2

three :: Natural
three = intToNat 3

testFunctor :: TestSuite
testFunctor = test "functor" do
    let dist = uniform [1, 10]
        res = uniform [2, 11]
    A.equal (map (add 1) dist) res

testApply :: TestSuite
testApply = test "apply" do
    let f = uniform [(add 1), (add 2)]
        dist = odds [(Tuple 1 one), (Tuple 10 two)]
        res = odds [(Tuple 2 one), (Tuple 11 two), (Tuple 3 one), (Tuple 12 two)]
    A.equal (apply f dist) res

testBind :: TestSuite
testBind = test "bind" (A.equal (sort $ condense $ bind dist f) (sort res))
    where
        f :: Boolean -> SP Boolean
        f true = uniform [true, false]
        f false = odds [(Tuple true one), (Tuple false zero)]

        dist :: SP Boolean
        dist = uniform [true, false]

        res :: SP Boolean
        res = odds [(Tuple true three), (Tuple false one)]

tests :: TestSuite
tests =
    suite "SimpleProb" do
        testFunctor
        testApply
        testBind
