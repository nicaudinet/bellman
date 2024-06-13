module Main where

import Prelude

import Data.Array.NonEmpty as NA
import Data.Foldable (intercalate)
import Data.List (List(..), (:))
import Data.Natural (Natural, intToNat, (+-))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- import Generation (GenM, State, Action, Value, zeroValue, next, reward, measure, possiblePolicies, showPolicy, initState, sort)
import RoomWalk (GenM, State, Action, Value, zeroValue, next, reward, measure, possiblePolicies, showPolicy, initState, sort)

-- Task: Implement backwards induction and apply it to 2-3 examples
-- * Solver needs to be independent of the particular SDP
-- * We can assume that the state and action spaces are finite
-- * Provide some test cases and expected outputs

-- 1) Compute an optimal policy sequence (for n=3)
-- 2) Compute all trajectories (or a sample)
-- 3) Sort them by probability
-- 4) Show the first few examples
-- 5) Say something about the computational complexity

type Policy = State -> Action 

type PolicySeq = List Policy

type XYPair = Tuple State Action

data XYSeq = Last State | Seq XYPair XYSeq

instance Show XYSeq where
    show (Last s) = show s
    show (Seq (Tuple s a) rest) = show s <> " -> " <> show a <> " -> " <> show rest

trajectory :: PolicySeq -> State -> GenM XYSeq
trajectory Nil s = pure (Last s)
trajectory (Cons p ps) s = do
    let y = p s
    s' <- next s y
    map (Seq (Tuple s y)) (trajectory ps s')

head :: XYSeq -> State
head (Last x) = x
head (Seq (Tuple x _) _) = x

sumReward :: XYSeq -> Value
sumReward (Last _) = zeroValue
sumReward (Seq (Tuple x y) rest) = reward x y (head rest) + sumReward rest

value :: PolicySeq -> State -> Value
value ps = trajectory ps >>> map sumReward >>> measure

valueBellman :: PolicySeq -> State -> Value
valueBellman Nil _ = 0.0
valueBellman (Cons p ps) x =
    let y = p x
        mx' = next x y
     in measure (map (\x' -> reward x y x' + valueBellman ps x') mx')

optimalExtension :: PolicySeq -> Policy
optimalExtension ps s = (NA.last (NA.sortWith fn possiblePolicies)) s
    where
        fn :: Policy -> Value
        fn p = valueBellman (p : ps) s

bi :: Natural -> PolicySeq
bi n =
    if n == zero
    then Nil
    else let ps = bi (n +- one) in optimalExtension ps : ps

showPolicySeq :: PolicySeq -> String
showPolicySeq = map showPolicy >>> intercalate " -> "

forM_ :: forall m a b. Monad m => List a -> (a -> m b) -> m Unit
forM_ Nil _ = pure unit 
forM_ (Cons x xs) f = f x >>= \_ -> forM_ xs f

main :: Effect Unit
main = do
    let policySeq = bi (intToNat 1)
    log (showPolicySeq policySeq)
    let ts = trajectory policySeq initState
    log (show (sort ts))
