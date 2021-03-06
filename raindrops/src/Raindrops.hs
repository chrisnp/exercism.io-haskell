module Raindrops (convert) where

convert :: Int -> String
convert n = 
    case noisy n of
        [] -> 
            show n
        factors -> 
            concat $ map sounds factors

sounds :: (Eq a, Num a) => a -> [Char]
sounds x  
    | x == 3    = "Pling" 
    | x == 5    = "Plang" 
    | x == 7    = "Plong" 
    | otherwise = "Error"

noisy :: Integral a => a -> [a]
noisy = 
    flip filter [3, 5, 7] . ((0 ==) .) . mod