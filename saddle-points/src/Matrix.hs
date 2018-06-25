module Matrix (saddlePoints) where

import Data.Array

-- saddlePoints :: Array i e -> [i]
saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = filter isSaddle coordinates
    where 
        isSaddle :: (Int, Int) -> Bool
        isSaddle point = elem == rowMax && elem == colMin
            where 
                elem = (matrix !) point
                rowMax = maximum (row point)
                colMin = minimum (col point)
        coordinates :: [(Int, Int)]
        coordinates = indices matrix
        row :: (Int, Int) -> [Int]
        row = getElem fst
        col :: (Int, Int) -> [Int]
        col = getElem snd
        getElem :: ((Int, Int) -> Int) -> (Int, Int) -> [Int]
        getElem extract point = map (matrix !) points
                where points = filter ((== extract point) . extract) coordinates