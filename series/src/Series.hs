module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)
    
slices :: Int -> String -> [[Int]]
slices n xs = 
    let
        partitions = take ((length xs) - n + 1) $ tails xs
    in 
        map (map digitToInt . take n) partitions