module CollatzConjecture (collatz) where

divBy2 :: Integer -> Integer
divBy2 num = num `div` 2

times3plus1 :: Integer -> Integer 
times3plus1 num = ( 3 * num ) + 1

collatzSteps :: Integer -> [Integer]
collatzSteps 1 = [1]
collatzSteps num 
    | even num  = num : collatzSteps ( divBy2 num ) 
    | otherwise = num : collatzSteps ( times3plus1 num )

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz num 
    | num > 1   = Just $ toInteger $ length (collatzSteps num) - 1
    | otherwise = Nothing 
