module IsbnVerifier (isbn) where

import Data.Char

isbn :: String -> Bool
isbn "" = False
isbn s = 
    let
        csum = 
            fmap sum . sequence . map isbnCh .zip [0..]
        valid xs = case csum xs of
            Just n  -> n `mod` 11 == 0
            Nothing -> False
        isbnCh (9, 'X')  = Just 10
        isbnCh (idx, d)
            | isNumber d = 
                Just $ (digitToInt d) * (10 - idx)
            | otherwise  = 
                Nothing
    in 
        valid (filter (/='-') s) && 
        length (filter (/='-') s) == 10 
