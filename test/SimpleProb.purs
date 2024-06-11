module Test.SimpleProb (tests) where

import Prelude

import Data.List (List(..), (:))
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert as A 

import SimpleProb (SP(..), PI(..))

testFunctor :: TestSuite
testFunctor = test "functor" do
    let sp = SP ((PI 1 one) : (PI 10 one) : Nil)
    A.equal (map (add 1) sp) (SP ((PI 2 one) : (PI 11 one) : Nil))

tests :: TestSuite
tests =
    suite "SimpleProb" do
        testFunctor
