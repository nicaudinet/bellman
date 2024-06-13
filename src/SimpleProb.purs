module SimpleProb
    ( SP
    , uniform
    , odds
    , condense
    , sort
    , psort
    , mean
    ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Either (Either(..))
import Data.Foldable (and, sum, intercalate)
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
    show (PI x p) = "(" <> show x <> " : " <> show p <> ")"

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
    show (SP xs) = intercalate "\n\t" ("SP" A.: map show xs)

instance (Eq a) => Eq (SP a) where
    eq :: SP a -> SP a -> Boolean
    eq (SP xs) (SP ys) = and (A.zipWith eq xs ys)

instance Functor SP where
    map :: forall a b. (a -> b) -> SP a -> SP b
    map f (SP xs) = SP (map (map f) xs)

instance Apply SP where
    apply :: forall a b. SP (a -> b) -> SP a -> SP b
    apply (SP fs) (SP xs) = SP ((map apply fs) <*> xs)

instance Applicative SP where
    pure :: forall a. a -> SP a
    pure x = SP (A.singleton (PI x 1.0))

instance Bind SP where
    bind :: forall a b. SP a -> (a -> SP b) -> SP b
    bind (SP xs) f = SP (A.concatMap (map f >>> scale >>> unSP) xs)
        where
          scale :: PI (SP b) -> SP b
          scale (PI sp p) = pmap (mul p) sp

instance Monad SP

unSP :: forall a. SP a -> Array (PI a)
unSP (SP xs) = xs

pmap :: forall a. (Number -> Number) -> SP a -> SP a
pmap f (SP xs) = SP (map (\(PI x p) -> PI x (f p)) xs)

-- | Creating distributions

uniform :: forall a. Array a -> SP a
uniform xs =
    let l = toNumber (A.length xs)
     in SP (map (\x -> PI x (1.0 / l)) xs)

odds :: forall a. Array (Tuple a Natural) -> SP a
odds xs =
    let natToNum = natToInt >>> toNumber
        total = natToNum (sum (map snd xs))
     in SP (map (\(Tuple x n) -> PI x (natToNum n / total)) xs)

-- | Rearranging distribution internals

condense :: forall a. Ord a => SP a -> SP a
condense = unSP >>> A.groupAllBy cmp >>> map foldGroups >>> SP
    where
        cmp :: PI a -> PI a -> Ordering
        cmp (PI x _) (PI y _) = compare x y
        foldGroups :: NonEmptyArray (PI a) -> PI a
        foldGroups xs = PI (item (head xs)) (sum (map prob xs))

sort :: forall a. Ord a => SP a -> SP a
sort = unSP >>> A.sortWith item >>> SP

psort :: forall a. SP a -> SP a
psort = unSP >>> A.sortWith prob >>> A.reverse >>> SP

-- | Functions on distributions

mean :: SP Number -> Number
mean (SP xs) = sum (map (\(PI x p) -> x * p) xs)
