module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)
    
slices :: Int -> String -> [[Int]]
slices n xs = map (map digitToInt . take n) partitions
    where 
        partitions = take ((length xs) - n + 1) $ tails xs
