module Sieve (primesUpTo) where


primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n <= 1 = []
  | otherwise = 2 : sieve [3, 5 .. n]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = 
    let 
        multiples n upto = 
            [ 2*n, 3*n .. upto ]
        productOf d m = 
            (or . map (== m)) (d : multiples d m) 
    in
        x : sieve (filter (not . productOf x) xs)
