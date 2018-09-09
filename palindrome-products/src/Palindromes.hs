module Palindromes (largestPalindrome, smallestPalindrome) where

reversed :: Integer -> Integer
reversed num = foldl (\acc x -> 10 * acc + x) 0 (digits num) 
    where
        digits 0 = []
        digits n = remainder : digits quotient 
            where 
                (quotient, remainder) = n `quotRem` 10

isPalindrome :: Integer -> Bool
isPalindrome number = number == reversed number

palindromeProduct :: (Integer, Integer) -> Bool
palindromeProduct = isPalindrome . uncurry (*)

findFactors :: Integer  -> Integer  -> Integer  -> [(Integer , Integer )]
findFactors number minFactor maxFactor = [ (x, div number x) | x <- smallest, maxFactor >= div number x]
    where
        limit = min maxFactor (floor $ (sqrt :: Double -> Double) $ fromIntegral number)
        smallest = [y | y <- [ minFactor .. limit ], 0 == mod number y]

largestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor = (largest, findFactors largest minFactor maxFactor)
    where
        factors = [ x * y |
                    x <- [minFactor .. maxFactor],
                    y <- [x .. maxFactor] ]
        largest = maximum $ filter isPalindrome factors

smallestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor = (smallest, findFactors smallest minFactor maxFactor)
    where 
        factors = [ x * y | 
                    x <- [minFactor .. maxFactor],
                    y <- [x .. maxFactor] ]
        smallest = minimum $ filter isPalindrome factors
      