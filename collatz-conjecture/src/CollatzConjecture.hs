module CollatzConjecture (collatz) where


collatzSteps :: Integer -> [Integer]
collatzSteps 1 = [1]
collatzSteps num 
    | even num  = num : collatzSteps ( num `quot` 2 ) 
    | otherwise = num : collatzSteps ( ( 3 * num ) + 1 )


collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz num 
    | num > 1   = Just $ toInteger $ length (collatzSteps num) - 1
    | otherwise = Nothing