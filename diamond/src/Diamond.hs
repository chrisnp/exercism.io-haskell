module Diamond (diamond) where

import Data.Char

diamond :: Char -> Maybe [String]
diamond 'A' = Just ["A"] 
diamond  ch 
    | not (ch `elem` ['A'..'Z']) = Nothing
    | otherwise = Just [draw n i | i <- [0 .. n] ++ [n - 1, n - 2 .. 0]]
    where 
        n = ord ch - ord 'A'
        draw x 0 = (replicate x ' ') ++ ['A'] ++ (replicate x ' ')
        draw x j = (replicate (x - j) ' ') ++ 
                   [chr (j + ord 'A')] ++ 
                   (replicate (j * 2 - 1) ' ') ++ 
                   [chr (j + ord 'A')] ++ 
                   (replicate (x - j) ' ')