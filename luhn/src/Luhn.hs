module Luhn (isValid) where

isValid :: Integer -> Bool
isValid n = checksum n == 0
    where
        checksum x = 

