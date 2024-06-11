module Main where

import Prelude

import Control.Monad.Except.Trans (ExceptT, throwError, runExceptT)
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM, sum)
import Data.Int (toNumber)
import Data.List (List(..), fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- Task: Implement backwards induction and apply it to 2-3 examples
-- * Solver needs to be independent of the particular SDP
-- * We can assume that the state and action spaces are finite
-- * Provide some test cases and expected outputs

-- 1) Compute an optimal policy sequence (for n=3)
-- 2) Compute all trajectories (or a sample)
-- 3) Sort them by probability
-- 4) Show the first few examples
-- 5) Say something about the computational complexity

-- Peano natural numbers
data Nat = Zero | Succ Nat

instance Semiring Nat where
    zero = Zero
    one = Succ Zero
    add Zero n = n
    add (Succ m) n = Succ (add m n)
    mul Zero _ = Zero
    mul (Succ m) n = add n (mul m n)

-- Generation problem:

data State
    = BP -- Bad Permanent
    | BT -- Bad Temporary
    | GU -- Good Unstabl
    | GS -- Good Stable

instance Show State where
    show BP = "BP"
    show BT = "BT"
    show GU = "GU"
    show GS = "GS"

data Action
    = GoBP
    | GoBT
    | GoGU
    | GoGS

instance Show Action where
    show GoBP = "GoBP"
    show GoBT = "GoBT"
    show GoGU = "GoGU"
    show GoGS = "GoGS"

type Value = Number

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

natToValue :: Nat -> Value
natToValue = natToInt >>> toNumber

badValue :: Value
badValue = toNumber (-1)

goodValue :: Value
goodValue = toNumber 1

data InvalidAction = InvalidAction State Action

instance Show InvalidAction where
    show (InvalidAction s a) = "Invalid action " <> show a <> " in state " <> show s

type GenM a = ExceptT InvalidAction List a

next :: State -> Action -> GenM State
next BP GoBP = pure BP
next BT GoGS = pure GS
next GU GoBP = pure BP
next GU GoBT = pure BT
next GU GoGU = pure GU
next GS GoGS = pure GS
next state action = throwError (InvalidAction state action)

reward :: State -> Action -> State -> Value
reward _ _ BP = badValue
reward _ _ BT = badValue
reward _ _ GS = goodValue
reward _ _ GU = goodValue

type Policy = State -> Action 

policy :: Policy
policy BP = GoBP
policy GS = GoGS
policy BT = GoGS
policy GU = GoBT

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
sumReward (Last _) = toNumber 0
sumReward (Seq (Tuple x y) rest) = reward x y (head rest) + sumReward rest

length :: forall a. List a -> Nat
length Nil = Zero
length (Cons _ xs) = Succ (length xs)

mean :: List Value -> Value
mean xs = sum xs / natToValue (length xs)

rights :: forall a b. List (Either a b) -> List b
rights Nil = Nil
rights (Cons (Left _) xs) = rights xs
rights (Cons (Right x) xs) = Cons x (rights xs)

measure :: GenM Value -> Value
measure = runExceptT >>> rights >>> mean

value :: PolicySeq -> State -> Value
value ps = trajectory ps >>> map sumReward >>> measure

main :: Effect Unit
main = do
    let policies = fromFoldable (replicate 5 policy)
        init = GU
    log $ show (runExceptT $ runPolicySeq init policies)
    log $ show (runExceptT $ trajectory policies GU)
    log $ show (runExceptT $ map sumReward $ trajectory policies GU)
    log $ show (value policies GU)
