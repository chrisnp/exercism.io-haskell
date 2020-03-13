module CollatzConjecture (collatz) where

import Data.List 

collatzStep :: Integer -> Integer
collatzStep num 
    | even num  = 
        num `quot` 2 
    | otherwise = 
        (3 * num ) + 1

collatz :: Integer -> Maybe Integer
collatz num  
    | num > 0 = 
        toInteger <$> 
        ( elemIndex 1 $ iterate collatzStep num ) 
    | otherwise = 
        Nothing 
