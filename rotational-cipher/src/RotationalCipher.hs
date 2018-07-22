module RotationalCipher (rotate) where

convert :: Int -> Char -> Char
convert key char 
    | 65 <= i && i <= 90  = toEnum $ (i + key - fromEnum 'A') `mod` 26 + fromEnum 'A' :: Char
    | 97 <= i && i <= 122 = toEnum $ (i + key - fromEnum 'a') `mod` 26 + fromEnum 'a' :: Char
    | otherwise           = char
    where i = fromEnum char


rotate :: Int -> String -> String
rotate key = map $ convert key
