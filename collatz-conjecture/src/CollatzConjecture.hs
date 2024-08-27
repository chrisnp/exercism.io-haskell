module CollatzConjecture (collatz) where

import Data.List
import Control.Monad ( liftM2 )

collatzStep :: Integer -> Integer
collatzStep = 
    (liftM2 if' even (`quot` 2)) 
    <*> 
    ((1 +) . (3 *))

collatz :: Integer -> Maybe Integer
collatz = 
    flip 
    (liftM2 if' (> 0) ((toInteger <$>) . elemIndex 1 . iterate collatzStep))
    Nothing

-- Auxiliary

if' :: Bool -> p -> p -> p
if' True  x _ = x
if' False _ y = y
