module SumOfMultiples (sumOfMultiples) where

import Data.List(nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] limit      = 0
sumOfMultiples factors limit = 
    sum $ distinctFactors factors limit

distinctFactors :: [Integer] -> Integer -> [Integer]
distinctFactors [] limit     = []
distinctFactors (x:xs) limit = 
    nub $ (distinctFactors xs limit) ++ (appendFactor x limit 1)  

appendFactor :: Integer -> Integer -> Integer -> [Integer]
appendFactor factor limit index 
    | factor * index >= limit = []
    | factor == 0 = []
    | otherwise = 
        (factor * index) : appendFactor factor limit (index + 1)