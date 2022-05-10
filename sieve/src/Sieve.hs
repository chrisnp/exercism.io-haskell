module Sieve (primesUpTo) where

import Control.Monad ( liftM2 )
import Control.Applicative ( (<*>) )
import Data.Bool ( bool )

primesUpTo :: (Ord a, Num a, Enum a) => a -> [a]
primesUpTo = (flip bool ([]) . (2 :) . sieve . enumFromThenTo 3 5) <*> (<= 1)

sieve :: (Eq a, Num a, Enum a) => [a] -> [a]
sieve [] = []
sieve (x:xs) = 
    let 
        multiples :: (Num a, Enum a) => a -> a -> [a]
        multiples = liftM2 enumFromThenTo (2 *) (3 *)
        productOf :: (Eq a, Num a, Enum a) => a -> a -> Bool
        productOf = liftM2 any (==) . liftM2 (.) (:) multiples
    in
        x : sieve (filter (not . productOf x) xs)
