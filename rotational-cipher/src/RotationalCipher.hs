module RotationalCipher (rotate) where

convert :: Int -> Char -> Char
convert key char 
    | i `elem` [65..90]  = toEnum $ (i + key - 65) `mod` 26 + 65 :: Char
    | i `elem` [97..122] = toEnum $ (i + key - 97) `mod` 26 + 97 :: Char
    | otherwise          = char
    where i = fromEnum char


rotate :: Int -> String -> String
rotate key = map $ convert key
