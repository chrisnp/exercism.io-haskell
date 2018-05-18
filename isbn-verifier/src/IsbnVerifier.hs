module IsbnVerifier (isbn) where

import Data.Char

isbn :: String -> Bool
isbn "" = False
isbn s  = length (filter (/='-') s) == 10 && valid (filter (/='-') s)
    where
    sum' xs = fmap sum $ sequence $ map isbnCh (zip [0..] xs)
    valid  xs = case sum' xs of
        Just n  -> n `mod` 11 == 0
        Nothing -> False
    isbnCh (9, 'X')  = Just 10
    isbnCh (idx, d)
        | isNumber d = Just $ (digitToInt d) * (10 - idx)
        | otherwise  = Nothing