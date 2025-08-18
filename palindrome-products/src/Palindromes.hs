module Palindromes (
                     largestPalindrome
                   , smallestPalindrome
                   ) 
where

import Data.List (unfoldr)

type ProductOfFactors = (Integer, [(Integer, Integer)])

largestPalindrome :: Integer -> Integer -> Maybe ProductOfFactors
largestPalindrome minFactor maxFactor = 
    let 
        products = generateProducts minFactor maxFactor
        palindromes = filter isPalindrome products
    in
        if minFactor > maxFactor then Nothing
        else if null palindromes then Nothing
        else 
            let 
                largest = maximum palindromes
             in 
                Just (largest, findFactors largest minFactor maxFactor)

smallestPalindrome :: Integer -> Integer -> Maybe ProductOfFactors
smallestPalindrome minFactor maxFactor = 
    let 
        products = generateProducts minFactor maxFactor
        palindromes = filter isPalindrome products
    in
        if minFactor > maxFactor then Nothing
        else if null palindromes then Nothing
        else 
            let 
                smallest = minimum palindromes
             in 
                Just (smallest, findFactors smallest minFactor maxFactor)

-- Auxiliary 

generateProducts :: Integer -> Integer -> [Integer]
generateProducts minF maxF = 
    concatMap (flip map [minF..maxF] . (*)) [minF..maxF]

isPalindrome :: Integer -> Bool
isPalindrome = (==) <*> reversed

reversed :: Integer -> Integer
reversed = 
    let
        digitExtractor 0 = Nothing
        digitExtractor n = Just (swap (quotRem n 10))
        swap (a, b) = (b, a)
    in
        foldl ((+) . (* 10)) 0 . unfoldr digitExtractor

findFactors :: Integer -> Integer -> Integer -> [(Integer, Integer)]
findFactors number minF maxF = 
    let 
        limit = (min maxF . floor . sqrt . fromIntegral) number
        inRange = enumFromTo minF limit
        isDivisor = ((== 0) . mod number)
        hasValidPair x = 
            let 
                y = div number x
            in 
                y >= minF && y <= maxF && x <= y
        makePair = (,) <*> div number
    in 
        (map makePair . filter hasValidPair . filter isDivisor) inRange
