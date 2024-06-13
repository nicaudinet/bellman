module Generation
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
    , initState
    , sort
    )
    where

import Prelude

import Data.Array.NonEmpty as NA
import Data.Int (toNumber)
import Data.Natural (intToNat)
import Data.Tuple (Tuple(..))

import SimpleProb (SP, odds, mean, psort)

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

possiblePolicies :: NA.NonEmptyArray (State -> Action)
possiblePolicies = (NA.singleton stay) <> (NA.singleton go)
    where
        stay = const Stay
        go x = if x == GU then Go else Stay

type Value = Number

zeroValue :: Value
zeroValue = 0.0

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
next GU Stay = odds [(Tuple GU (intToNat 9)), (Tuple BP one)]
next GU Go = pure BT

reward :: State -> Action -> State -> Value
reward _ _ BP = badValue
reward _ _ BT = badValue
reward _ _ GS = goodValue
reward _ _ GU = goodValue

measure :: GenM Value -> Value
measure = mean

showPolicy :: (State -> Action) -> String
showPolicy p = if p GU == Stay then "Stay" else "Go"

initState :: State
initState = GU

sort :: forall a. GenM a -> GenM a
sort = psort
