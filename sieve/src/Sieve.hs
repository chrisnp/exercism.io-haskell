module Sieve (primesUpTo) where

import Control.Monad ( liftM2 )

primesUpTo :: (Ord a, Num a, Enum a) => a -> [a]
primesUpTo = 
    app (flip if' ([]) . (<= 1)) ((2 :) . sieve . enumFromThenTo 3 5)

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

-- Auxiliary

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

app :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
app f g = \x -> f x (g x)
