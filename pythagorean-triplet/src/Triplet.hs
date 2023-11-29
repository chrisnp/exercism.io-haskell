module Triplet (tripletsWithSum) where

import Data.List (sortBy)

isPythagorean :: (Integral a) => (a, a, a) -> Bool
isPythagorean (x, y, z) = 
    let 
        squared =
            map (^ 2) $ sortBy (\a b -> compare b a) [x, y, z]
    in 
        sum squared == 2 * (head squared)

mkTriplet :: (Integral a) => a -> a -> a -> (a, a, a)
mkTriplet = (,,)

pythagoreanTriplets :: (Integral a) => a -> a -> [(a, a, a)]
pythagoreanTriplets minFactor maxFactor = 
    let 
        triplets =  [mkTriplet a b c | 
                    a <- [minFactor .. maxFactor], 
                    b <- [a .. maxFactor], 
                    c <- [b .. maxFactor]]
    in 
        filter isPythagorean triplets

tripletsWithSum :: (Integral a) =>  a -> [(a, a, a)]
tripletsWithSum s = 
    [(a, b, c) | a <- [1..div s 3],
                 b <- [a + 1..2 * (div s 3)],
                 c <- [s - a - b],
                 a < b, 
                 b < c,
                 isPythagorean (a, b, c)]
