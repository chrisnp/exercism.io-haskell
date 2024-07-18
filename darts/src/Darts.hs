module Darts (score) where

score :: Float -> Float -> Int
score x y
    | radius > 10 = 0
    | radius >  5 = 1
    | radius >  1 = 1 + 4
    | otherwise   = 1 + 4 + 5
    where 
        radius = (x ** 2 + y ** 2) ** 0.5
