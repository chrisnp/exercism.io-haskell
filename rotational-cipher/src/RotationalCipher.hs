module RotationalCipher (rotate) where

convert :: Int -> Char -> Char
convert key char 
    | au <= c && c <= zu = 
            toEnum $ (c + key - au) `mod` 26 + au :: Char
    | al <= c && c <= zl = 
            toEnum $ (c + key - al) `mod` 26 + al :: Char
    | otherwise = char
    where 
        c  = fromEnum char
        au = fromEnum 'A'
        zu = fromEnum 'Z'
        al = fromEnum 'a'
        zl = fromEnum 'z'


rotate :: Int -> String -> String
rotate = map . convert
