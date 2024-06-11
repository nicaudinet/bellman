module SimpleProb where

import Prelude

import Data.Foldable (and)
import Data.List (List(..), (:), zipWith)
import Data.Natural (Natural) 

-- A probability item : an item with an associated probability
data PI a = PI a Natural

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

-- instance Bind PI where
--     bind :: forall a b. PI a -> (a -> PI b) -> PI b
--     bind 

-- Simple (finite) probability distributions
data SP a = SP (List (PI a))

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
    pure x = SP ((PI x one) : Nil)

-- instance Bind SP where 
--     bind :: forall a b. SP a -> (a -> SP b) -> SP b
--     bind (SP xs) f =
