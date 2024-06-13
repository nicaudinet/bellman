module RoomWalk
    ( GenM
    , State(..)
    , Action(..)
    , Value
    , zeroValue
    , next
    , reward
    , measure
    , possiblePolicies
    , showPolicy
    )
    where

import Prelude

import Data.Array.NonEmpty as NA
import Data.Foldable (intercalate)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Natural (intToNat)
import Data.Tuple (Tuple(..))

import SimpleProb (SP, mean, odds)

-- Random Walk problem:

type GenM a = SP a

--   ----------------
-- 2 |    |    | +1 |
--   ----------------
-- 1 | ST |    | -1 |
--   ----------------
--     1   2   3

data State = C11 | C12 | C21 | C22 | C31 | C32

instance Show State where
    show :: State -> String
    show C11 = "C11"
    show C12 = "C12"
    show C21 = "C21"
    show C22 = "C22"
    show C31 = "C31"
    show C32 = "C32"

data Action = U | D | L | R

instance Show Action where
    show :: Action -> String
    show U = "^"
    show D = "v"
    show L = "<"
    show R = ">"

type Policy =
    { c11 :: Action
    , c12 :: Action
    , c21 :: Action
    , c22 :: Action
    , c31 :: Action
    , c32 :: Action
    }

policyToFn :: Policy -> (State -> Action)
policyToFn p C11 = p.c11
policyToFn p C12 = p.c22
policyToFn p C21 = p.c21
policyToFn p C22 = p.c32
policyToFn p C31 = p.c31
policyToFn p C32 = p.c32

possibleActions :: Array Action
possibleActions = [U, D, L, R]

possiblePolicies :: NA.NonEmptyArray (State -> Action)
possiblePolicies = fromMaybe (NA.singleton (const U)) $ NA.fromArray do
    c11 <- possibleActions
    c12 <- possibleActions
    c21 <- possibleActions
    c22 <- possibleActions
    c31 <- possibleActions
    c32 <- possibleActions
    pure (policyToFn { c11, c12, c21, c22, c31, c32 })

type Value = Number

zeroValue :: Value
zeroValue = 0.0

badValue :: Value
badValue = toNumber (-1)

goodValue :: Value
goodValue = toNumber 1

next :: State -> Action -> GenM State
-- C11
next C11 U = odds [(Tuple C12 (intToNat 8)), (Tuple C11 one), (Tuple C21 one)] 
next C11 D = odds [(Tuple C11 (intToNat 9)), (Tuple C21 one)] 
next C11 L = odds [(Tuple C11 (intToNat 9)), (Tuple C12 one)] 
next C11 R = odds [(Tuple C21 (intToNat 8)), (Tuple C11 one), (Tuple C12 one)] 
-- C12
next C12 U = odds [(Tuple C12 (intToNat 9)), (Tuple C22 one)] 
next C12 D = odds [(Tuple C11 (intToNat 8)), (Tuple C12 one), (Tuple C22 one)] 
next C12 L = odds [(Tuple C12 (intToNat 9)), (Tuple C11 one)] 
next C12 R = odds [(Tuple C22 (intToNat 8)), (Tuple C11 one), (Tuple C12 one)] 
-- C21
next C21 U = odds [(Tuple C22 (intToNat 8)), (Tuple C11 one), (Tuple C31 one)] 
next C21 D = odds [(Tuple C22 (intToNat 8)), (Tuple C11 one), (Tuple C31 one)] 
next C21 L = odds [(Tuple C11 (intToNat 8)), (Tuple C22 one), (Tuple C21 one)] 
next C21 R = odds [(Tuple C31 (intToNat 8)), (Tuple C22 one), (Tuple C21 one)] 
-- C22
next C22 U = odds [(Tuple C22 (intToNat 8)), (Tuple C12 one), (Tuple C32 one)] 
next C22 D = odds [(Tuple C21 (intToNat 8)), (Tuple C12 one), (Tuple C32 one)] 
next C22 L = odds [(Tuple C12 (intToNat 8)), (Tuple C22 one), (Tuple C21 one)] 
next C22 R = odds [(Tuple C32 (intToNat 8)), (Tuple C22 one), (Tuple C21 one)] 
-- C31
next C31 U = odds [(Tuple C32 (intToNat 8)), (Tuple C21 one), (Tuple C31 one)] 
next C31 D = odds [(Tuple C31 (intToNat 9)), (Tuple C21 one)] 
next C31 L = odds [(Tuple C21 (intToNat 8)), (Tuple C31 one), (Tuple C32 one)] 
next C31 R = odds [(Tuple C31 (intToNat 9)), (Tuple C32 one)] 
-- C32
next C32 U = odds [(Tuple C32 (intToNat 9)), (Tuple C22 one)] 
next C32 D = odds [(Tuple C31 (intToNat 8)), (Tuple C22 one), (Tuple C32 one)] 
next C32 L = odds [(Tuple C22 (intToNat 8)), (Tuple C31 one), (Tuple C32 one)] 
next C32 R = odds [(Tuple C32 (intToNat 9)), (Tuple C31 one)] 

reward :: State -> Action -> State -> Value
reward _ _ C32 = goodValue
reward _ _ C31 = badValue
reward _ _ _ = zeroValue

measure :: GenM Value -> Value
measure = mean

showPolicy :: (State -> Action) -> String
showPolicy p = intercalate "\n"
    [ "-------------"
    , "| " <> show (p C12) <> " | " <> show (p C22) <> " | " <> show (p C32) <> " |"
    , "-------------"
    , "| " <> show (p C11) <> " | " <> show (p C21) <> " | " <> show (p C31) <> " |"
    , "-------------"
    ]
