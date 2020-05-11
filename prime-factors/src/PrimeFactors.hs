module PrimeFactors (primeFactors) where

factors :: Integral a => a -> [a] -> [a]
factors 1 _  = 
    []
factors n [] = 
    [n]
factors n (x:xs) 
    | n `mod` x == 0 = 
        x : factors (n `quot` x) (x:xs)
    | otherwise = 
        factors n xs

primeFactors :: Integral a => a -> [a]
primeFactors = 
    factors <*> ( enumFromTo 2 
                  . floor 
                  . sqrt 
                  . fromIntegral )