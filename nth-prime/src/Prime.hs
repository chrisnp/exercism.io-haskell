module Prime (nth) where

nth :: Int -> Maybe Integer
nth n 
    | n < 1     = Nothing
    | otherwise = Just (primes !! (pred n)) 

primes :: [Integer]
primes = 
    let 
        isPrime p = all ((/=) 0 . mod p) $ takeWhile (\n -> n * n <= p) primes
    in
        2 : filter isPrime [3, 5..]