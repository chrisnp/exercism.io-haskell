module IsbnVerifier (isbn) where

import Data.Char
import Data.Maybe
import Control.Monad

-- isbnVal :: (Int, Char) -> Maybe Int
-- isbnVal (idx, d) 
--     | (9, 'X')   = Just 10 
--     | isNumber d = Just $ (digitToInt d) * (10 - idx) 
--     | otherwise  = Nothing

-- isbnSum :: 
-- isbnSum = fmap sum $ sequence $ map isbnValue (zip [0..])


isbn :: String -> Bool
isbn "" = False
-- isbn s  = length (valid s) == 10 && isbnSum `mod` 11 == 0 && 'X' `notElem` (init $ valid s) 
isbn s = valid (filter (/='-') s)
    where
        isbnSum xs = fmap sum $ sequence $ map isbnVal (zip [0..] xs)
        valid  xs = case isbnSum xs of
            Just total -> total `mod` 11 == 0
            Nothing -> False
        isbnVale (9, 'X')      = Just 10
        isbnVal (index, digit)
            | isNumber digit   = Just $ (digitToInt digit) * (10 - index)
            | otherwise        = Nothing



