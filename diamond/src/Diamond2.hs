module Diamond2 (diamond) where

import Data.Char

diamond :: Char -> Maybe [String]
diamond  ch 
    | ch `elem` ['A'..'Z'] = Just $ pattern ch
    | otherwise            = Nothing
    where 
        pattern ch  = map row area
        row rowNum  = map char $ zip (repeat rowNum) area
        char (row, at) 
            | at == delta         = charX
            | at == width - delta = charX
            | otherwise           = ' '
            where 
                charX = chr $ ord ch - delta
                delta = abs $ intVal - row
        intVal = ord ch - ord 'A'
        width  = 2 * intVal
        area = [0..width] 