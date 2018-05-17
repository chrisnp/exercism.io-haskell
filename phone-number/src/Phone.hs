module Phone (number) where

import Data.Char

number :: String -> Maybe String
number xs 
    | length (clean xs) == 10 = valid (clean xs)
    | length (clean xs) == 11 && head (clean xs) == '1' = valid $ tail (clean xs)
    | otherwise                                         = Nothing
    where clean = filter isDigit
          valid ys 
            | (ys !! 0) `elem` ['2'..'9'] && (ys !! 3) `elem` ['2'..'9'] = Just ys
            | otherwise                                                = Nothing