module RotationalCipher (rotate) where

convert :: Int -> Char -> Char
convert key char 
    | fromEnum 'A' <= i && i <= fromEnum 'Z' = 
            toEnum $ (i + key - fromEnum 'A') `mod` 26 + fromEnum 'A' :: Char
    | fromEnum 'a' <= i && i <= fromEnum 'z' = 
            toEnum $ (i + key - fromEnum 'a') `mod` 26 + fromEnum 'a' :: Char
    | otherwise = char
    where i = fromEnum char


rotate :: Int -> String -> String
rotate key = map $ convert key
