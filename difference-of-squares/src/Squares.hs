module Squares (difference, squareOfSum, sumOfSquares) where

import Control.Monad ( liftM2 )

difference :: Integral a => a -> a
difference = 
    liftM2 (-) squareOfSum sumOfSquares

squareOfSum :: Integral a => a -> a
squareOfSum = 
    (^ 2) . (`div` 2) . (<*>) (*) (1 +)

sumOfSquares :: Integral a => a -> a
sumOfSquares = 
    (`div` 6) . (<*>) ((*) . (<*>) (*) (1 +)) ((1 +) . (2 *))