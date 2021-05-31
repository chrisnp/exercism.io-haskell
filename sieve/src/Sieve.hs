module Sieve (primesUpTo) where


primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise = 2 : sieve [3,5 .. n]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = 
    let 
        isMultiple p q = 
            (or . map (== q)) [p, p+p .. q] 
    in
        x:sieve (filter (not . isMultiple x) xs)
