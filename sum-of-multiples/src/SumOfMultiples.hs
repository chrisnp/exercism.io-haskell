module SumOfMultiples (sumOfMultiples) where

import Data.List(nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] limit = 
    0
sumOfMultiples factors limit = 
    sum $ distinctFactors factors limit

distinctFactors :: (Integral a) => [a] -> a -> [a]
distinctFactors [] limit = 
    []
distinctFactors (x:xs) limit = 
    nub $ 
    (distinctFactors xs limit) ++ 
    (appendFactor x limit 1)  

appendFactor :: (Integral a) => a -> a -> a -> [a]
appendFactor factor limit index 
    | factor * index >= limit = 
        []
    | factor == 0 = 
        []
    | otherwise = 
        (factor * index) : 
        appendFactor factor limit (index + 1)