module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = 
    let 
        primes = 
            2 : filter isPrime [3, 5..]
        isPrime p = 
            all ((/=) 0 . mod p) 
                (takeWhile (\x ->  x ^ 2 <= p) 
                           primes)
    in 
        takeWhile (<= n) primes