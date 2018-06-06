module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n = Just $ roman n

    
roman :: Integer -> String
roman n 
    | n >= 1000 = "M" ++ roman (n - 1000)
    | n >= 900  = "CM" ++ roman (n - 900)
    | n >= 500  = "D" ++ roman (n - 500)
    | n >= 400  = "CD" ++ roman (n - 400)
    | n >= 100  = "C" ++ roman (n - 100)
    | n >= 90   = "XC" ++ roman (n - 90)
    | n >= 50   = "L" ++ roman (n - 50)
    | n >= 40   = "XL" ++ roman (n - 40)
    | n >= 10   = "X" ++ roman (n - 10)
    | n >= 9    = "IX" ++ roman (n - 9)
    | n >= 5    = "V" ++ roman (n - 5)
    | n >= 4    = "IV" ++ roman (n - 4)
    | n >= 1    = "I" ++ roman (n - 1)
    | n == 0    = ""
    | otherwise = error "not well formed arabic " ++ show n