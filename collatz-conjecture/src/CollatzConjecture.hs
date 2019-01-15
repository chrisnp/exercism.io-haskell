module CollatzConjecture (collatz) where


collatzSteps :: Integer -> [Integer]
collatzSteps 1 = [1]
collatzSteps num 
    | even num  = num : collatzSteps ( divBy2 num ) 
    | otherwise = num : collatzSteps ( times3plus1 num )
    where
        divBy2 n = n `div` 2
        times3plus1 n = ( 3 * n ) + 1


collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz num 
    | num > 1   = Just $ toInteger $ length (collatzSteps num) - 1
    | otherwise = Nothing