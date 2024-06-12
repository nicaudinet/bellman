module SimpleProb
    ( SP
    , uniform
    , odds
    ) where

import Prelude

import Data.Array (singleton, length, zipWith)
import Data.Foldable (and, sum)
import Data.Int (toNumber)
import Data.Natural (Natural, natToInt)
import Data.Tuple (Tuple(..), snd)

-- A probability item : an item with an associated probability
-- Probabilities are represented as Javascript Numbers
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Data_structures#number_type
data PI a = PI a Number

item :: forall a. PI a -> a
item (PI x _) = x

prob :: forall a. PI a -> Number
prob (PI _ p) = p

instance Show a => Show (PI a) where
    show :: PI a -> String
    show (PI x p) = show x <> " : " <> show p

instance (Eq a) => Eq (PI a) where
    eq :: PI a -> PI a -> Boolean
    eq (PI x p) (PI y q) = x == y && p == q

instance Functor PI where
    map :: forall a b. (a -> b) -> PI a -> PI b
    map f (PI x p) = PI (f x) p

instance Apply PI where
    apply :: forall a b. PI (a -> b) -> PI a -> PI b
    apply (PI f p) (PI x q) = PI (f x) (mul p q)

-- Simple (finite) probability distributions
data SP a = SP (Array (PI a))

instance (Show a) => Show (SP a) where
    show :: SP a -> String
    show (SP xs) = show xs

instance (Eq a) => Eq (SP a) where
    eq :: SP a -> SP a -> Boolean
    eq (SP xs) (SP ys) = and (zipWith eq xs ys)

instance Functor SP where
    map :: forall a b. (a -> b) -> SP a -> SP b
    map f (SP xs) = SP (map (map f) xs)

instance Apply SP where
    apply :: forall a b. SP (a -> b) -> SP a -> SP b
    apply (SP fs) (SP xs) = SP ((map apply fs) <*> xs)

instance Applicative SP where
    pure :: forall a. a -> SP a
    pure x = SP (singleton (PI x 1.0))

uniform :: forall a. Array a -> SP a
uniform xs =
    let l = toNumber (length xs)
     in SP (map (\x -> PI x (1.0 / l)) xs)

odds :: forall a. Array (Tuple a Natural) -> SP a
odds xs =
    let natToNum = natToInt >>> toNumber
        total = natToNum (sum (map snd xs))
     in SP (map (\(Tuple x n) -> PI x (natToNum n / total)) xs)
