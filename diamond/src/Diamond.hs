module Diamond (diamond) where

import Data.Char

diamond :: Char -> Maybe [String]
diamond 'A' = Just ["A"] 
diamond  ch 
    | not $ ch `elem` ['A'..'Z'] = Nothing
    | otherwise                  = Just [draw n i | i <- [0 .. n] ++ [n - 1, n - 2 .. 0]]
    where 
        n = ord ch - ord 'A'
        draw n 0 = (replicate n ' ') ++ ['A'] ++ (replicate n ' ')
        draw n i = (replicate (n - i) ' ') ++ [chr (i + ord 'A')] ++ (replicate (i * 2 - 1) ' ') ++ [chr (i + ord 'A')] ++ (replicate (n - i) ' ')
