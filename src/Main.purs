module Main where

import Prelude

import Data.Array (replicate)
import Data.Foldable (class Foldable, foldM, intercalate)
import Data.Int (toNumber)
import Data.Natural (Natural, intToNat, (+-))
import Data.List (List(..), (:), fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

import SimpleProb (SP, odds, mean)

-- Task: Implement backwards induction and apply it to 2-3 examples
-- * Solver needs to be independent of the particular SDP
-- * We can assume that the state and action spaces are finite
-- * Provide some test cases and expected outputs

-- 1) Compute an optimal policy sequence (for n=3)
-- 2) Compute all trajectories (or a sample)
-- 3) Sort them by probability
-- 4) Show the first few examples
-- 5) Say something about the computational complexity

-- Generation problem:

data State
    = BP -- Bad Permanent
    | BT -- Bad Temporary
    | GS -- Good Stable
    | GU -- Good Unstable

instance Show State where
    show BP = "BP"
    show BT = "BT"
    show GS = "GS"
    show GU = "GU"

instance Eq State where
    eq BP BP = true
    eq BT BT = true
    eq GS GS = true
    eq GU GU = true
    eq _ _ = false

data Action
    = Stay -- Stay in current node
    | Go -- Go to other node (only applies to GU)

instance Show Action where
    show Stay = "Stay"
    show Go = "Go"

instance Eq Action where
    eq Stay Stay = true
    eq Go Go = true
    eq _ _ = false

type Value = Number

badValue :: Value
badValue = toNumber (-1)

goodValue :: Value
goodValue = toNumber 1

data InvalidAction = InvalidAction State Action

instance Show InvalidAction where
    show (InvalidAction s a) = "Invalid action " <> show a <> " in state " <> show s

type GenM a = SP a

next :: State -> Action -> GenM State
next BP _ = pure BP
next BT _ = pure GS
next GS _ = pure GS
next GU Stay = odds [(Tuple GU (intToNat 4)), (Tuple BP one)]
next GU Go = pure BT

reward :: State -> Action -> State -> Value
reward _ _ BP = badValue
reward _ _ BT = badValue
reward _ _ GS = goodValue
reward _ _ GU = goodValue

type Policy = State -> Action 

policy :: Policy
policy BP = Stay
policy BT = Stay
policy GS = Stay
policy GU = Stay

runPolicy :: State -> Policy -> GenM State
runPolicy s p = next s (p s)

type PolicySeq = List Policy

accumulateM
    :: forall m a b
     . Monad m
    => (b -> a -> m b) -> Tuple b (List b) -> a -> m (Tuple b (List b))
accumulateM f (Tuple b bs) a = do
    b' <- f b a
    pure (Tuple b' (Cons b' bs))

foldAccumM
    :: forall f m a b
     . Foldable f
    => Monad m
    => (b -> a -> m b) -> b -> f a -> m (Tuple b (List b))
foldAccumM f init = foldM (accumulateM f) (Tuple init (Cons init Nil))

runPolicySeq :: State -> PolicySeq -> GenM (Tuple State (List State))
runPolicySeq = foldAccumM runPolicy

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
sumReward (Last _) = 0.0
sumReward (Seq (Tuple x y) rest) = reward x y (head rest) + sumReward rest

measure :: GenM Value -> Value
measure = mean

value :: PolicySeq -> State -> Value
value ps = trajectory ps >>> map sumReward >>> measure

valueBellman :: PolicySeq -> State -> Value
valueBellman Nil _ = 0.0
valueBellman (Cons p ps) x =
    let y = p x
        mx' = next x y
     in measure (map (\x' -> reward x y x' + valueBellman ps x') mx')

optimalExtension :: PolicySeq -> Policy
optimalExtension ps s =
    if valueBellman (stay : ps) s >= valueBellman (go : ps) s
    then stay s
    else go s
    where
        stay :: Policy
        stay = const Stay

        go :: Policy
        go GU = Go
        go _ = Stay

bi :: Natural -> PolicySeq
bi n =
    if n == zero
    then Nil
    else let ps = bi (n +- one) in optimalExtension ps : ps

showPolicy :: Policy -> String
showPolicy p = if p GU == Stay then "Stay" else "Go"

showPolicySeq :: PolicySeq -> String
showPolicySeq = map showPolicy >>> intercalate " -> "

main :: Effect Unit
main = do
    let policies = fromFoldable (replicate 5 policy)
        init = GU
    log $ show (runPolicySeq init policies)
    log $ show (trajectory policies GU)
    log $ show (map sumReward $ trajectory policies GU)
    log $ show (value policies GU)
    log $ show (valueBellman policies GU)
    log $ showPolicySeq (bi (intToNat 0))
    log $ showPolicySeq (bi (intToNat 1))
    log $ showPolicySeq (bi (intToNat 2))
    log $ showPolicySeq (bi (intToNat 3))
    log $ showPolicySeq (bi (intToNat 4))
    log $ showPolicySeq (bi (intToNat 5))
    log $ showPolicySeq (bi (intToNat 6))
    log $ showPolicySeq (bi (intToNat 7))
    log $ showPolicySeq (bi (intToNat 8))
