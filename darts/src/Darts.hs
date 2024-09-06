module Darts (score) where

score :: Float -> Float -> Int
score x y =
    let 
        radius = (x ** 2 + y ** 2) ** 0.5
        points  | radius > 10 = 0
                | radius >  5 = 1
                | radius >  1 = 1 + 4
                | otherwise   = 1 + 4 + 5
     in 
        points 
