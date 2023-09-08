module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum = 
    (^ 2) . (`div` 2) . (<*>) (*) (1 +)

sumOfSquares :: Integral a => a -> a
sumOfSquares = 
    (`div` 6) . (<*>) ((*) . (<*>) (*) (1 +)) ((1 +) . (2 *))