module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List ( sort )

data PythagoreanTriplet = PythagoreanTriplet {a::Int, b::Int, c::Int} deriving (Show, Eq)

isPythagorean :: PythagoreanTriplet -> Bool
isPythagorean triplet = ((a triplet) ^ (2::Integer)) + ((b triplet) ^ (2::Integer)) == (c triplet) ^ (2::Integer)

mkTriplet :: Int -> Int -> Int -> PythagoreanTriplet
mkTriplet a b c = PythagoreanTriplet (head sorted) (sorted !! 1) (sorted !! 2)
    where sorted = sort [a, b, c]

pythagoreanTriplets :: Int -> Int -> [PythagoreanTriplet]
pythagoreanTriplets minFactor maxFactor = filter isPythagorean triplets
    where triplets =  [mkTriplet a b c | a <- [minFactor .. maxFactor], b <- [a .. maxFactor], c <- [b .. maxFactor]]
